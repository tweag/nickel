use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::PathBuf,
    process::Child,
    time::{Duration, Instant},
};

use crossbeam::channel::{Receiver, Select, Sender};
use ipc_channel::ipc::{IpcOneShotServer, IpcReceiver, IpcSender};
use nickel_lang_core::{
    cache::{Cache, CacheError, SourcePath},
    error::IntoDiagnostics as _,
    eval::{cache::CacheImpl, VirtualMachine},
};
use serde::{Deserialize, Serialize};

use crate::{
    analysis::AnalysisRegistry,
    cache::CacheExt as _,
    diagnostic::{DiagnosticCompat as _, SerializableDiagnostic},
    utils::initialize_stdlib,
};

const EVAL_TIMEOUT: Duration = Duration::from_secs(1);

#[derive(Debug, Serialize, Deserialize)]
enum Command {
    UpdateFile { path: PathBuf, text: String },
    EvalFile { path: PathBuf },
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
        path: PathBuf,
    },
}

pub struct BackgroundJobs {
    receiver: Receiver<Diagnostics>,
    sender: Sender<Command>,
}

pub fn worker_main(main_server: String) {
    let oneshot_tx = IpcSender::connect(main_server).unwrap();
    let (cmd_tx, cmd_rx) = ipc_channel::ipc::channel().unwrap();
    let (response_tx, response_rx) = ipc_channel::ipc::channel().unwrap();
    dbg!(oneshot_tx.send((cmd_tx, response_rx))).unwrap();

    worker(cmd_rx, response_tx);
}

fn drain_ready<T: for<'de> Deserialize<'de> + Serialize>(rx: &IpcReceiver<T>, buf: &mut Vec<T>) {
    while let Ok(x) = rx.try_recv() {
        buf.push(x);
    }
}

// Returning an Option, just to let us use `?` when channels disconnect.
fn worker(cmd_rx: IpcReceiver<Command>, response_tx: IpcSender<Response>) -> Option<()> {
    let mut evals = VecDeque::new();
    let mut cmds = Vec::new();
    let mut cache = Cache::new(nickel_lang_core::cache::ErrorTolerance::Tolerant);

    if let Ok(nickel_path) = std::env::var("NICKEL_IMPORT_PATH") {
        cache.add_import_paths(nickel_path.split(':'));
    }
    cache.load_stdlib().unwrap();

    // TODO: we shouldn't need analysis in the background worker, but some of our utilities assume we have one...
    let mut analysis = AnalysisRegistry::default();

    let initial_ctxt = cache.mk_type_ctxt().unwrap();
    let initial_env = initialize_stdlib(&mut cache, &mut analysis);

    drain_ready(&cmd_rx, &mut cmds);
    loop {
        for cmd in cmds.drain(..) {
            // Process all the file updates first, even if it's out of order with the evals.
            // (Is there any use case for wanting the eval before updating the contents?)
            match cmd {
                Command::UpdateFile { path, text } => {
                    cache.replace_string(SourcePath::Path(path), text);
                }
                Command::EvalFile { path } => {
                    evals.push_back(path);
                }
            }
        }

        // Deduplicate the evals back-to-front, and then evaluate front-to-back.
        // This means that if we get requests for `foo.ncl` and `bar.ncl`, and
        // then another request for `foo.ncl` before started working on the
        // first one, then we'll throw away the first `foo.ncl` and evaluate
        // `bar.ncl` first.
        let mut seen = HashSet::new();
        let mut dedup = VecDeque::new();
        for path in evals.iter().rev() {
            if seen.insert(path) {
                dedup.push_front(path.clone());
            }
        }
        evals = dedup;

        if let Some(path) = evals.pop_front() {
            // TODO: factor out this inner block
            if let Some(file_id) = cache.id_of(&SourcePath::Path(path.clone())) {
                response_tx
                    .send(dbg!(Response::Starting { path: path.clone() }))
                    .ok()?;

                let (parse_errs, fatal) = match cache.parse(file_id) {
                    Ok(errs) => (errs.inner(), false),
                    Err(errs) => (errs, true),
                };
                let mut diags = parse_errs.into_diagnostics(cache.files_mut(), None);

                if !fatal {
                    if let Err(CacheError::Error(errors)) = cache.typecheck_with_analysis(
                        file_id,
                        &initial_ctxt,
                        &initial_env,
                        &mut analysis,
                    ) {
                        diags.extend(
                            errors
                                .into_iter()
                                .flat_map(|e| e.into_diagnostics(cache.files_mut(), None)),
                        );
                    }
                }

                // Evaluation diagnostics (but only if there were no parse/type errors).
                if diags.is_empty() {
                    // TODO: avoid cloning the cache.
                    let mut vm =
                        VirtualMachine::<_, CacheImpl>::new(cache.clone(), std::io::stderr());
                    let rt = vm.prepare_eval(file_id).unwrap(); // FIXME
                    if let Err(e) = vm.eval_full(rt) {
                        diags.extend(e.into_diagnostics(cache.files_mut(), None));
                    }
                }

                let diagnostics: Vec<_> = diags
                    .into_iter()
                    .flat_map(|d| SerializableDiagnostic::from_codespan(d, cache.files_mut()))
                    .collect();

                // If there's been an update to the file, don't send back a stale response.
                cmds.extend(cmd_rx.try_recv());
                if !cmds.iter().any(|cmd| match cmd {
                    Command::UpdateFile { path: p, .. } => p == &path,
                    _ => false,
                }) {
                    response_tx
                        .send(Response::Diagnostics(Diagnostics { path, diagnostics }))
                        .ok()?;
                }
            }
        }

        drain_ready(&cmd_rx, &mut cmds);
        if cmds.is_empty() {
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

    contents: HashMap<PathBuf, String>,

    // If evaluating a file causes the worker to time out or crash, we blacklist that file
    // and refuse to evaluate it anymore. This could be relaxed (e.g. maybe we're willing to
    // try again after a certain amount of time?).
    banned_files: HashSet<PathBuf>,
    eval_in_progress: Option<(PathBuf, Instant)>,
}

enum SupervisorError {
    MainExited,
    WorkerExited,
    WorkerTimedOut(PathBuf),
}

impl SupervisorState {
    fn spawn_worker() -> anyhow::Result<(Child, IpcSender<Command>, Receiver<Response>)> {
        // TODO: don't panic
        let path = std::env::current_exe()?;
        let (oneshot_server, server_name) =
            IpcOneShotServer::<(IpcSender<Command>, IpcReceiver<Response>)>::new()?;
        let child = std::process::Command::new(path)
            .args(["--main-server", &server_name])
            .spawn()
            .unwrap();

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
        if let Command::UpdateFile { path, text } = &msg {
            self.contents.insert(path.clone(), text.clone());
        }
        if let Command::EvalFile { path } = &msg {
            if self.banned_files.contains(path) {
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
            Response::Starting { path } => {
                let timeout = Instant::now() + EVAL_TIMEOUT;
                self.eval_in_progress = Some((path, timeout));
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
                    self.handle_response(resp).ok().unwrap();
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
                path: path.clone(),
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
                        self.restart_worker().unwrap(); // FIXME
                    }
                }
                Err(SupervisorError::WorkerTimedOut(path)) => {
                    self.banned_files.insert(path);
                    self.eval_in_progress = None;
                    let _ = self.child.kill();
                    self.restart_worker().unwrap(); // FIXME
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

    pub fn update_file(&mut self, path: PathBuf, text: String) {
        let _ = self.sender.send(Command::UpdateFile { path, text });
    }

    pub fn eval_file(&mut self, path: PathBuf) {
        self.sender.send(Command::EvalFile { path }).unwrap();
    }

    pub fn receiver(&self) -> &Receiver<Diagnostics> {
        &self.receiver
    }
}
