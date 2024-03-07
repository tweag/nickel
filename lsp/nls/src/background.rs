use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    process::Child,
    time::{Duration, Instant},
};

use crossbeam::channel::{Receiver, Select, Sender};
use ipc_channel::ipc::{IpcOneShotServer, IpcReceiver, IpcSender};
use log::warn;
use lsp_types::Url;
use nickel_lang_core::{
    cache::{ImportResolver, SourcePath},
    error::EvalError,
    eval::{
        cache::{Cache, CacheImpl},
        VirtualMachine,
    },
    term::{RichTerm, RuntimeContract, Term},
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

// The entry point of the background worker. If it fails to bootstrap the connection,
// panic immediately (it's a subprocess anyway).
pub fn worker_main(main_server: String) {
    let oneshot_tx = IpcSender::connect(main_server).unwrap();
    let (cmd_tx, cmd_rx) = ipc_channel::ipc::channel().unwrap();
    let (response_tx, response_rx) = ipc_channel::ipc::channel().unwrap();
    oneshot_tx.send((cmd_tx, response_rx)).unwrap();

    worker(cmd_rx, response_tx);
}

fn drain_ready<T: for<'de> Deserialize<'de> + Serialize>(rx: &IpcReceiver<T>, buf: &mut Vec<T>) {
    while let Ok(x) = rx.try_recv() {
        buf.push(x);
    }
}

// Evaluate `rt` and collect errors.
//
// This differs from `VirtualMachine::eval_full` in 2 ways:
// - We try to accumulate errors instead of bailing out. When recursing into record
//   fields and array elements, we keep evaluating subsequent elements even if one
//   fails.
// - We ignore missing field errors. It would be nice not to ignore them, but it's hard
//   to tell when they're appropriate: the term might intentionally be a partial configuration.
fn eval_permissive(
    vm: &mut VirtualMachine<impl ImportResolver, impl Cache>,
    errors: &mut Vec<EvalError>,
    rt: RichTerm,
) {
    match vm.eval(rt) {
        Err(e) => errors.push(e),
        Ok(t) => match t.as_ref() {
            Term::Array(ts, _) => {
                for t in ts.iter() {
                    // After eval_closure, all the array elements  are
                    // closurized already, so we don't need to do any tracking
                    // of the env.
                    eval_permissive(vm, errors, t.clone());
                }
            }
            Term::Record(data) => {
                for field in data.fields.values() {
                    if let Some(v) = &field.value {
                        let value_with_ctr = RuntimeContract::apply_all(
                            v.clone(),
                            field.pending_contracts.iter().cloned(),
                            v.pos,
                        );
                        eval_permissive(vm, errors, value_with_ctr);
                    }
                }
            }
            _ => {}
        },
    }
}

// Returning an Option, just to let us use `?` when channels disconnect.
fn worker(cmd_rx: IpcReceiver<Command>, response_tx: IpcSender<Response>) -> Option<()> {
    let mut evals = Vec::new();
    let mut cmds = Vec::new();
    let mut world = World::default();

    loop {
        for cmd in cmds.drain(..) {
            // Process all the file updates first, even if it's out of order with the evals.
            // (Is there any use case for wanting the eval before updating the contents?)
            match cmd {
                Command::UpdateFile { uri, text } => {
                    // Failing to update a file's contents is bad, so we terminate (and restart)
                    // the worker.
                    world.update_file(uri, text).unwrap();
                }
                Command::EvalFile { uri } => {
                    evals.push(uri);
                }
            }
        }

        // Deduplicate the evals back-to-front, and then evaluate front-to-back.
        // This means that if we get requests for `foo.ncl` and `bar.ncl`, and
        // then another request for `foo.ncl` before started working on the
        // first one, then we'll throw away the first `foo.ncl` and evaluate
        // `bar.ncl` first.
        let mut seen = HashSet::new();
        let mut dedup = Vec::new();
        for path in evals.iter().rev() {
            if seen.insert(path) {
                dedup.push(path.clone());
            }
        }
        evals = dedup;

        if let Some(uri) = evals.pop() {
            let Ok(path) = uri_to_path(&uri) else {
                warn!("skipping invalid uri {uri}");
                continue;
            };

            if let Some(file_id) = world.cache.id_of(&SourcePath::Path(path.clone())) {
                response_tx
                    .send(Response::Starting { uri: uri.clone() })
                    .ok()?;

                let mut diagnostics = world.parse_and_typecheck(file_id);

                // Evaluation diagnostics (but only if there were no parse/type errors).
                if diagnostics.is_empty() {
                    // TODO: avoid cloning the cache.
                    let mut vm =
                        VirtualMachine::<_, CacheImpl>::new(world.cache.clone(), std::io::stderr());
                    // We've already checked that parsing and typechecking are successful, so we
                    // don't expect further errors.
                    let rt = vm.prepare_eval(file_id).unwrap();
                    let mut errors = Vec::new();
                    eval_permissive(&mut vm, &mut errors, rt);
                    diagnostics.extend(
                        errors
                            .into_iter()
                            .flat_map(|e| world.lsp_diagnostics(file_id, e)),
                    );
                }

                // If there's been an update to the file, don't send back a stale response.
                cmds.extend(cmd_rx.try_recv());
                if !cmds.iter().any(|cmd| match cmd {
                    Command::UpdateFile { uri, .. } => {
                        uri_to_path(uri).map_or(false, |p| p == path)
                    }
                    _ => false,
                }) {
                    response_tx
                        .send(Response::Diagnostics(Diagnostics { path, diagnostics }))
                        .ok()?;
                }
            }
        }

        drain_ready(&cmd_rx, &mut cmds);
        if cmds.is_empty() && evals.is_empty() {
            // Wait for a command to be available.
            cmds.push(cmd_rx.recv().ok()?);
        }
    }
}

struct SupervisorState {
    cmd_rx: Receiver<Command>,
    response_tx: Sender<Diagnostics>,
    child: Child,
    cmd_tx: IpcSender<Command>,
    response_rx: Receiver<Response>,

    contents: HashMap<Url, String>,

    // If evaluating a file causes the worker to time out or crash, we blacklist that file
    // and refuse to evaluate it anymore. This could be relaxed (e.g. maybe we're willing to
    // try again after a certain amount of time?).
    banned_files: HashSet<Url>,
    eval_in_progress: Option<(Url, Instant)>,
}

enum SupervisorError {
    MainExited,
    WorkerExited,
    WorkerTimedOut(Url),
}

impl SupervisorState {
    fn spawn_worker() -> anyhow::Result<(Child, IpcSender<Command>, Receiver<Response>)> {
        let path = std::env::current_exe()?;
        let (oneshot_server, server_name) =
            IpcOneShotServer::<(IpcSender<Command>, IpcReceiver<Response>)>::new()?;
        let child = std::process::Command::new(path)
            .args(["--main-server", &server_name])
            .spawn()?;

        let (_, (ipc_cmd_tx, ipc_response_rx)) = oneshot_server.accept()?;
        let ipc_response_rx = ipc_channel::router::ROUTER
            .route_ipc_receiver_to_new_crossbeam_receiver(ipc_response_rx);
        Ok((child, ipc_cmd_tx, ipc_response_rx))
    }

    fn new(cmd_rx: Receiver<Command>, response_tx: Sender<Diagnostics>) -> anyhow::Result<Self> {
        let (child, cmd_tx, response_rx) = SupervisorState::spawn_worker()?;
        Ok(Self {
            cmd_rx,
            response_tx,
            contents: HashMap::new(),
            child,
            cmd_tx,
            response_rx,
            banned_files: HashSet::new(),
            eval_in_progress: None,
        })
    }

    fn handle_command(&mut self, msg: Command) -> Result<(), SupervisorError> {
        if let Command::UpdateFile { uri, text } = &msg {
            self.contents.insert(uri.clone(), text.clone());
        }
        if let Command::EvalFile { uri } = &msg {
            if self.banned_files.contains(uri) {
                return Ok(());
            }
        }

        self.cmd_tx
            .send(msg)
            .map_err(|_| SupervisorError::WorkerExited)
    }

    fn handle_response(&mut self, msg: Response) -> Result<(), SupervisorError> {
        match msg {
            Response::Diagnostics(d) => {
                self.eval_in_progress = None;
                self.response_tx
                    .send(d)
                    .map_err(|_| SupervisorError::MainExited)
            }
            Response::Starting { uri } => {
                let timeout = Instant::now() + EVAL_TIMEOUT;
                self.eval_in_progress = Some((uri, timeout));
                Ok(())
            }
        }
    }

    fn run_one(&mut self) -> Result<(), SupervisorError> {
        loop {
            let mut select = Select::new();
            select.recv(&self.cmd_rx);
            select.recv(&self.response_rx);

            let op = if let Some((path, timeout)) = &self.eval_in_progress {
                select
                    .select_deadline(*timeout)
                    .map_err(|_| SupervisorError::WorkerTimedOut(path.clone()))?
            } else {
                select.select()
            };
            match op.index() {
                0 => {
                    let cmd = op
                        .recv(&self.cmd_rx)
                        .map_err(|_| SupervisorError::MainExited)?;
                    self.handle_command(cmd)?;
                }
                1 => {
                    let resp = op
                        .recv(&self.response_rx)
                        .map_err(|_| SupervisorError::WorkerExited)?;
                    self.handle_response(resp)?;
                }
                _ => unreachable!(),
            }
        }
    }

    fn restart_worker(&mut self) -> anyhow::Result<()> {
        let (child, cmd_tx, response_rx) = Self::spawn_worker()?;
        self.cmd_tx = cmd_tx;
        self.child = child;
        self.response_rx = response_rx;

        // Tell the worker about all the files we know about.
        // Currently, we don't restart any evals that were queued up before the failure. Maybe
        // we should?
        for (path, contents) in self.contents.iter() {
            self.cmd_tx.send(Command::UpdateFile {
                uri: path.clone(),
                text: contents.clone(),
            })?;
        }

        Ok(())
    }

    fn run(&mut self) {
        loop {
            match self.run_one() {
                Ok(_) => unreachable!(),
                Err(SupervisorError::MainExited) => break,
                Err(SupervisorError::WorkerExited) => {
                    if let Some((path, _)) = self.eval_in_progress.take() {
                        self.banned_files.insert(path);
                        if let Err(e) = self.restart_worker() {
                            warn!("failed to restart worker: {e}");
                            break;
                        }
                    }
                }
                Err(SupervisorError::WorkerTimedOut(path)) => {
                    self.banned_files.insert(path);
                    self.eval_in_progress = None;
                    let _ = self.child.kill();
                    if let Err(e) = self.restart_worker() {
                        warn!("failed to restart worker: {e}");
                        break;
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
                eprintln!("failed to spawn background jobs: {e}");
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
