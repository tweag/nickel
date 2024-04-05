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

use crate::{
    cache::CacheExt as _, diagnostic::SerializableDiagnostic, files::uri_to_path, world::World,
};

const EVAL_TIMEOUT: Duration = Duration::from_secs(1);
const RECURSION_LIMIT: usize = 128;

#[derive(Debug, Serialize, Deserialize)]
enum Command {
    UpdateFile {
        uri: Url,
        text: String,
        deps: Vec<Url>,
    },
    EvalFile {
        uri: Url,
    },
}

/// The evaluation data that gets sent to the background worker.
#[derive(Debug, Serialize, Deserialize)]
struct Eval {
    /// All contents of in-lsp-memory files that are needed for the evaluation. (Including
    /// the contents of the actual file to evaluate.)
    contents: Vec<(Url, String)>,
    /// The url of the file to evaluate.
    eval: Url,
}

/// A borrowed version of `Eval`
#[derive(Debug, Serialize)]
struct EvalRef<'a> {
    contents: Vec<(&'a Url, &'a str)>,
    eval: &'a Url,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Diagnostics {
    pub path: PathBuf,
    pub diagnostics: Vec<SerializableDiagnostic>,
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

// The entry point of the background worker. This background worker
// reads an `Eval` (in bincode) from stdin, performs the evaluation, and
// writes a `Diagnostics` (in bincode) to stdout.
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
            let errors = vm.eval_permissive(rt, RECURSION_LIMIT);
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

        diagnostics.sort();
        diagnostics.dedup();
        let diagnostics = Diagnostics { path, diagnostics };

        // If this fails, the main process has already exited. No need for a loud error in that case.
        let _ = bincode::serialize_into(std::io::stdout().lock(), &diagnostics);
    }

    Ok(())
}

struct SupervisorState {
    cmd_rx: Receiver<Command>,
    response_tx: Sender<Diagnostics>,

    contents: HashMap<Url, String>,
    deps: HashMap<Url, Vec<Url>>,

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
            deps: HashMap::new(),
            banned_files: HashSet::new(),
            eval_stack: Vec::new(),
        })
    }

    fn dependencies<'a>(&'a self, uri: &'a Url) -> HashSet<&'a Url> {
        let mut stack = vec![uri];
        let mut ret = std::iter::once(uri).collect::<HashSet<_>>();

        while let Some(uri) = stack.pop() {
            if let Some(deps) = self.deps.get(uri) {
                for dep in deps {
                    if self.contents.contains_key(dep) && ret.insert(dep) {
                        stack.push(dep);
                    }
                }
            }
        }

        ret
    }

    // Evaluate the nickel file with the given uri, blocking until it completes or times out.
    //
    // The current implementation uses a background process per invocation, which is not the
    // most efficient thing but it allows for cancellation and prevents memory leaks.
    fn eval(&self, uri: &Url) -> anyhow::Result<Diagnostics> {
        let path = std::env::current_exe()?;
        let mut child = std::process::Command::new(path)
            .arg("--background-eval")
            .stdout(std::process::Stdio::piped())
            .stdin(std::process::Stdio::piped())
            .spawn()?;

        let tx = child.stdin.take();
        let rx = child.stdout.take();

        scopeguard::defer! {
            // If we successfully deserialized the response, the child should be just about done anyway
            // (and killing an already-finished process isn't an error).
            // Otherwise, we might have timed out waiting for the child, so kill it to reclaim resources.
            if child.kill().is_ok() {
                // We should wait on the child process to avoid having zombies, but if the
                // kill failed then we skip waiting because we don't actually want to block.
                let _ = child.wait();
            }
        }

        let mut tx = tx.ok_or_else(|| anyhow!("failed to get worker stdin"))?;
        let rx = rx.ok_or_else(|| anyhow!("failed to get worker stdout"))?;

        let dependencies = self.dependencies(uri);
        let eval = EvalRef {
            contents: dependencies
                .iter()
                .filter_map(|&dep| self.contents.get(dep).map(|text| (dep, text.as_ref())))
                .collect(),
            eval: uri,
        };
        bincode::serialize_into(&mut tx, &eval)?;

        let result = run_with_timeout(move || bincode::deserialize_from(rx), EVAL_TIMEOUT);

        Ok(result??)
    }

    fn handle_command(&mut self, cmd: Command) {
        match cmd {
            Command::UpdateFile { uri, text, deps } => {
                self.contents.insert(uri.clone(), text);
                self.deps.insert(uri, deps);
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
                // This blocks until the eval is done. We allow further eval requests to queue up
                // in the channel while we're working.
                match self.eval(&uri) {
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

    pub fn update_file(&mut self, uri: Url, text: String, world: &World) {
        let Ok(Some(file_id)) = world.cache.file_id(&uri) else {
            return;
        };
        let deps = world
            .cache
            .get_imports(file_id)
            .filter_map(|dep_id| world.file_uris.get(&dep_id))
            .cloned()
            .collect();

        // Ignore errors here, because if we've failed to set up a background worker
        // then we just skip doing background evaluation.
        let _ = self.sender.send(Command::UpdateFile { uri, text, deps });
    }

    pub fn eval_file(&mut self, uri: Url) {
        let _ = self.sender.send(Command::EvalFile { uri });
    }

    pub fn receiver(&self) -> &Receiver<Diagnostics> {
        &self.receiver
    }
}
