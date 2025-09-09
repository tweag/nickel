use std::{
    collections::HashMap,
    path::PathBuf,
    time::{Duration, Instant},
};

use anyhow::anyhow;
use crossbeam::channel::{bounded, Receiver, RecvTimeoutError, Sender};
use log::warn;
use lsp_types::Url;
use nickel_lang_core::cache::{InputFormat, SourcePath};
use serde::{Deserialize, Serialize};

use crate::{config, diagnostic::SerializableDiagnostic, files::uri_to_path, world::World};

/// Environment variable used to pass the recursion limit value to the child worker
const RECURSION_LIMIT_ENV_VAR_NAME: &str = "NICKEL_NLS_RECURSION_LIMIT";

/// The evaluation data that gets sent to the background worker.
#[derive(Debug, Serialize, Deserialize)]
struct Eval {
    /// All contents of in-lsp-memory files that are needed for the evaluation. (Including
    /// the contents of the actual file to evaluate.)
    contents: Vec<(Url, String)>,
    /// The url of the file to evaluate.
    eval: Url,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Diagnostics {
    pub path: PathBuf,
    pub diagnostics: Vec<SerializableDiagnostic>,
}

pub struct BackgroundJobs {
    receiver: Option<Receiver<Diagnostics>>,
    sender: Sender<Eval>,
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
    let mut world = World::new();
    let eval: Eval = bincode::serde::decode_from_std_read(
        &mut std::io::stdin().lock(),
        bincode::config::standard(),
    )?;
    for (uri, text) in eval.contents {
        world.add_file(uri, text)?;
    }

    let Ok(path) = uri_to_path(&eval.eval) else {
        anyhow::bail!("skipping invalid uri {}", eval.eval);
    };

    if let Some(file_id) = world
        .sources
        .id_of(&SourcePath::Path(path.clone(), InputFormat::Nickel))
    {
        let recursion_limit = std::env::var(RECURSION_LIMIT_ENV_VAR_NAME)?.parse::<usize>()?;
        let diagnostics = world.eval_diagnostics(file_id, recursion_limit);
        let diagnostics = Diagnostics { path, diagnostics };

        // If this fails, the main process has already exited. No need for a loud error in that case.
        let _ = bincode::serde::encode_into_std_write(
            &diagnostics,
            &mut std::io::stdout().lock(),
            bincode::config::standard(),
        );
    }

    Ok(())
}

struct SupervisorState {
    eval_rx: Receiver<Eval>,
    response_tx: Sender<Diagnostics>,

    // A stack of files we want to evaluate, which we do in LIFO order.
    eval_stack: Vec<Eval>,

    // If evaluating a file causes the worker to time out or crash, we blacklist that file
    // and refuse to evaluate it for `self.config.blacklist_duration`
    banned_files: HashMap<Url, Instant>,

    config: config::LspEvalConfig,
}

impl SupervisorState {
    fn new(
        eval_rx: Receiver<Eval>,
        response_tx: Sender<Diagnostics>,
        config: config::LspEvalConfig,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            eval_rx,
            response_tx,
            banned_files: HashMap::new(),
            eval_stack: Vec::new(),
            config,
        })
    }

    // Evaluate the nickel file with the given uri, blocking until it completes or times out.
    //
    // The current implementation uses a background process per invocation, which is not the
    // most efficient thing but it allows for cancellation and prevents memory leaks.
    fn eval(&self, eval: &Eval) -> anyhow::Result<Diagnostics> {
        let path = std::env::current_exe()?;
        let mut child = std::process::Command::new(path)
            .env(
                RECURSION_LIMIT_ENV_VAR_NAME,
                self.config.eval_limits.recursion_limit.to_string(),
            )
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
        let mut rx = rx.ok_or_else(|| anyhow!("failed to get worker stdout"))?;

        bincode::serde::encode_into_std_write(eval, &mut tx, bincode::config::standard())?;

        let result = run_with_timeout(
            move || bincode::serde::decode_from_std_read(&mut rx, bincode::config::standard()),
            self.config.eval_limits.timeout,
        );

        Ok(result??)
    }

    fn handle_eval(&mut self, eval: Eval) {
        match self.banned_files.get(&eval.eval) {
            Some(blacklist_time) if blacklist_time.elapsed() < self.config.blacklist_duration => {}
            _ => {
                // If we re-request an evaluation, remove the old one. (This is quadratic in the
                // size of the eval stack, but it only contains unique entries so we don't expect it
                // to get big.)
                if let Some(idx) = self.eval_stack.iter().position(|u| u.eval == eval.eval) {
                    self.eval_stack.remove(idx);
                }
                self.eval_stack.push(eval)
            }
        }
    }

    fn drain_queue(&mut self) {
        for eval in self.eval_rx.try_iter().collect::<Vec<_>>() {
            self.handle_eval(eval);
        }
    }

    fn run(&mut self) {
        loop {
            if self.eval_stack.is_empty() {
                // Block until a eval request is available, to avoid busy-looping.
                match self.eval_rx.recv() {
                    Ok(eval) => self.handle_eval(eval),
                    // If the main process has exited, just exit quietly.
                    Err(_) => break,
                }
            }
            self.drain_queue();

            if let Some(eval) = self.eval_stack.pop() {
                // This blocks until the eval is done. We allow further eval requests to queue up
                // in the channel while we're working.
                match self.eval(&eval) {
                    Ok(diagnostics) => {
                        if self.response_tx.send(diagnostics).is_err() {
                            break;
                        }
                    }
                    Err(e) => {
                        // Most likely the background eval timed out (but it could be something
                        // more exotic, like failing to spawn the subprocess).
                        warn!("background eval failed: {e}");
                        self.banned_files.insert(eval.eval, Instant::now());
                    }
                }
            }
        }
    }
}

impl BackgroundJobs {
    pub fn new(config: config::LspEvalConfig) -> Self {
        let (eval_tx, eval_rx) = crossbeam::channel::unbounded();
        let (diag_tx, diag_rx) = crossbeam::channel::unbounded();

        if config.disable {
            Self {
                sender: eval_tx,
                receiver: None,
            }
        } else {
            match SupervisorState::new(eval_rx, diag_tx, config) {
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
                sender: eval_tx,
                receiver: Some(diag_rx),
            }
        }
    }

    fn contents(&self, uri: &Url, world: &World) -> Option<Vec<(Url, String)>> {
        let file_id = world.file_id(uri).ok()??;
        let mut contents: Vec<(Url, String)> = world
            .import_data
            .transitive_imports(file_id)
            .into_iter()
            .filter_map(|dep_id| {
                world
                    .file_uris
                    .get(&dep_id)
                    .map(|uri| (uri.clone(), world.sources.source(dep_id).to_string()))
            })
            .collect();

        contents.push((uri.clone(), world.sources.source(file_id).to_string()));

        Some(contents)
    }

    pub fn eval_file(&mut self, uri: Url, world: &World) {
        if let Some(contents) = self.contents(&uri, world) {
            let _ = self.sender.send(Eval {
                eval: uri,
                contents,
            });
        };
    }

    pub fn receiver(&self) -> Option<&Receiver<Diagnostics>> {
        self.receiver.as_ref()
    }
}
