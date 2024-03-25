use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    time::Duration,
};

use anyhow::anyhow;
use crossbeam::channel::{bounded, Receiver, RecvTimeoutError, Sender};
use log::warn;
use lsp_types::Url;
use nickel_lang_core::{
    cache::SourcePath,
    eval::{cache::CacheImpl, VirtualMachine},
};
use serde::{Deserialize, Serialize};

use crate::{diagnostic::SerializableDiagnostic, files::uri_to_path, world::World};

const EVAL_TIMEOUT: Duration = Duration::from_secs(1);

#[derive(Debug, Serialize, Deserialize)]
enum Command {
    UpdateFile { uri: Url, text: String },
    EvalFile { uri: Url },
}

#[derive(Debug, Serialize, Deserialize)]
struct Eval {
    contents: Vec<(Url, String)>,
    eval: Url,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Diagnostics {
    pub path: PathBuf,
    pub diagnostics: Vec<SerializableDiagnostic>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Response {
    Diagnostics(Diagnostics),
    /// The background worker sends back one of these when it's about to start
    /// an eval job. That way, if it becomes unresponsive we know which file is
    /// the culprit.
    Starting {
        uri: Url,
    },
}

pub struct BackgroundJobs {
    receiver: Receiver<Diagnostics>,
    sender: Sender<Command>,
}

fn run_with_timeout<T: Send + 'static, F: FnOnce() -> T + Send + 'static>(
    f: F,
    timeout: Duration,
) -> Result<T, RecvTimeoutError> {
    let (tx, rx) = bounded(1);
    std::thread::spawn(move || {
        let result = f();
        let _ = tx.send(result);
    });
    rx.recv_timeout(timeout)
}

// The entry point of the background worker. If it fails to bootstrap the connection,
// panic immediately (it's a subprocess anyway).
pub fn worker_main() -> anyhow::Result<()> {
    let mut world = World::default();
    let eval: Eval = bincode::deserialize_from(std::io::stdin().lock())?;
    for (uri, text) in eval.contents {
        world.add_file(uri, text)?;
    }

    let Ok(path) = uri_to_path(&eval.eval) else {
        anyhow::bail!("skipping invalid uri {}", eval.eval);
    };

    if let Some(file_id) = world.cache.id_of(&SourcePath::Path(path.clone())) {
        let mut diagnostics = world.parse_and_typecheck(file_id);

        // Evaluation diagnostics (but only if there were no parse/type errors).
        if diagnostics.is_empty() {
            // TODO: avoid cloning the cache.
            let mut vm =
                VirtualMachine::<_, CacheImpl>::new(world.cache.clone(), std::io::stderr());
            // We've already checked that parsing and typechecking are successful, so we
            // don't expect further errors.
            let rt = vm.prepare_eval(file_id).unwrap();
            let errors = vm.eval_permissive(rt);
            diagnostics.extend(
                errors
                    .into_iter()
                    .filter(|e| {
                        !matches!(
                            e,
                            nickel_lang_core::error::EvalError::MissingFieldDef { .. }
                        )
                    })
                    .flat_map(|e| world.lsp_diagnostics(file_id, e)),
            );
        }

        let diagnostics = Diagnostics { path, diagnostics };
        bincode::serialize_into(std::io::stdout().lock(), &diagnostics)?;
    }

    Ok(())
}

struct SupervisorState {
    cmd_rx: Receiver<Command>,
    response_tx: Sender<Diagnostics>,

    contents: HashMap<Url, String>,

    // A stack of files we want to evaluate, which we do in LIFO order.
    eval_stack: Vec<Url>,

    // If evaluating a file causes the worker to time out or crash, we blacklist that file
    // and refuse to evaluate it anymore. This could be relaxed (e.g. maybe we're willing to
    // try again after a certain amount of time?).
    banned_files: HashSet<Url>,
}

impl SupervisorState {
    fn new(cmd_rx: Receiver<Command>, response_tx: Sender<Diagnostics>) -> anyhow::Result<Self> {
        Ok(Self {
            cmd_rx,
            response_tx,
            contents: HashMap::new(),
            banned_files: HashSet::new(),
            eval_stack: Vec::new(),
        })
    }

    fn eval(&self, uri: Url) -> anyhow::Result<Diagnostics> {
        let path = std::env::current_exe()?;
        let mut child = std::process::Command::new(path)
            .arg("--background-eval")
            .env("RUST_BACKTRACE", "1")
            .stdout(std::process::Stdio::piped())
            .stdin(std::process::Stdio::piped())
            .spawn()?;
        let mut tx = child
            .stdin
            .take()
            .ok_or_else(|| anyhow!("failed to get worker stdin"))?;

        // TODO: we don't need to send *every* file, just the ones in the transitive dependency tree
        let eval = Eval {
            contents: self
                .contents
                .iter()
                .map(|(a, b)| (a.clone(), b.clone()))
                .collect(),
            eval: uri,
        };
        bincode::serialize_into(&mut tx, &eval)?;

        let result = run_with_timeout(move || child.wait_with_output(), EVAL_TIMEOUT)??;
        Ok(bincode::deserialize_from(std::io::Cursor::new(
            result.stdout,
        ))?)
    }

    fn handle_command(&mut self, cmd: Command) {
        match cmd {
            Command::UpdateFile { uri, text } => {
                self.contents.insert(uri, text);
            }
            Command::EvalFile { uri } => {
                if !self.banned_files.contains(&uri) {
                    // If we re-request an evaluation, remove the old one. (This is quadratic in the
                    // size of the eval stack, but it only contains unique entries so we don't expect it
                    // to get big.)
                    if let Some(idx) = self.eval_stack.iter().position(|u| u == &uri) {
                        self.eval_stack.remove(idx);
                    }
                    self.eval_stack.push(uri);
                }
            }
        }
    }

    fn drain_commands(&mut self) {
        for cmd in self.cmd_rx.try_iter().collect::<Vec<_>>() {
            self.handle_command(cmd);
        }
    }

    fn run(&mut self) {
        loop {
            if self.eval_stack.is_empty() {
                // Block until a command is available, to avoid busy-looping.
                match self.cmd_rx.recv() {
                    Ok(cmd) => self.handle_command(cmd),
                    // If the main process has exited, just exit quietly.
                    Err(_) => break,
                }
            }
            self.drain_commands();

            if let Some(uri) = self.eval_stack.pop() {
                match self.eval(uri.clone()) {
                    Ok(diagnostics) => {
                        if self.response_tx.send(diagnostics).is_err() {
                            break;
                        }
                    }
                    Err(e) => {
                        // Most likely the background eval timed out (but it could be something
                        // more exotic, like failing to spawn the subprocess).
                        warn!("background eval failed: {e}");
                        self.banned_files.insert(uri);
                    }
                }
            }
        }
    }
}

impl BackgroundJobs {
    pub fn new() -> Self {
        let (cmd_tx, cmd_rx) = crossbeam::channel::unbounded();
        let (diag_tx, diag_rx) = crossbeam::channel::unbounded();
        match SupervisorState::new(cmd_rx, diag_tx) {
            Ok(mut sup) => {
                std::thread::spawn(move || {
                    sup.run();
                });
            }
            Err(e) => {
                warn!("failed to spawn background jobs: {e}");
            }
        }

        Self {
            sender: cmd_tx,
            receiver: diag_rx,
        }
    }

    pub fn update_file(&mut self, uri: Url, text: String) {
        // Ignore errors here, because if we've failed to set up a background worker
        // then we just skip doing background evaluation.
        let _ = self.sender.send(Command::UpdateFile { uri, text });
    }

    pub fn eval_file(&mut self, uri: Url) {
        let _ = self.sender.send(Command::EvalFile { uri });
    }

    pub fn receiver(&self) -> &Receiver<Diagnostics> {
        &self.receiver
    }
}
