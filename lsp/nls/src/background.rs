use std::{
    collections::{HashSet, VecDeque},
    path::PathBuf,
};

use crossbeam::channel::{Receiver, Sender};
use lsp_types::Diagnostic;
use nickel_lang_core::{
    cache::{Cache, CacheError, SourcePath},
    error::IntoDiagnostics as _,
    eval::{cache::CacheImpl, VirtualMachine},
};

use crate::{
    analysis::AnalysisRegistry, cache::CacheExt as _, diagnostic::DiagnosticCompat as _,
    utils::initialize_stdlib,
};

#[derive(Debug)]
enum Command {
    UpdateFile { path: PathBuf, text: String },
    EvalFile { path: PathBuf },
}

#[derive(Debug)]
pub struct Response {
    pub path: PathBuf,
    pub diagnostics: Vec<Diagnostic>,
}

pub struct BackgroundJobs {
    receiver: Receiver<Response>,
    sender: Sender<Command>,
}

fn worker(cmd_rx: Receiver<Command>, response_tx: Sender<Response>) -> Option<()> {
    let mut evals = VecDeque::new();
    let mut cmds: Vec<_> = cmd_rx.try_iter().collect();
    let mut cache = Cache::new(nickel_lang_core::cache::ErrorTolerance::Tolerant);

    if let Ok(nickel_path) = std::env::var("NICKEL_IMPORT_PATH") {
        cache.add_import_paths(nickel_path.split(':'));
    }
    cache.load_stdlib().unwrap();

    // TODO: we shouldn't need analysis in the background worker, but some of our utilities assume we have one...
    let mut analysis = AnalysisRegistry::default();

    let initial_ctxt = cache.mk_type_ctxt().unwrap();
    let initial_env = initialize_stdlib(&mut cache, &mut analysis);

    loop {
        for cmd in cmds.drain(..) {
            dbg!(&cmd);
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
            dbg!(&path);
            // TODO: factor out this inner block
            if let Some(file_id) = cache.id_of(&SourcePath::Path(path.clone())) {
                let (parse_errs, fatal) = match cache.parse(file_id) {
                    Ok(errs) => (errs.inner(), false),
                    Err(errs) => (errs, true),
                };
                dbg!(&parse_errs, fatal);
                let mut diags = parse_errs.into_diagnostics(cache.files_mut(), None);

                if !fatal {
                    if let Err(CacheError::Error(errors)) = dbg!(cache.typecheck_with_analysis(
                        file_id,
                        &initial_ctxt,
                        &initial_env,
                        &mut analysis
                    )) {
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

                let diagnostics = diags
                    .into_iter()
                    .flat_map(|d| Diagnostic::from_codespan(d, cache.files_mut()))
                    .collect();

                // If there's been an update to the file, don't send back a stale response.
                cmds.extend(cmd_rx.try_recv());
                if !cmds.iter().any(|cmd| match cmd {
                    Command::UpdateFile { path: p, .. } => p == &path,
                    _ => false,
                }) {
                    response_tx.send(Response { path, diagnostics }).ok()?;
                }
            }
        }

        cmds.extend(cmd_rx.try_recv());
        if cmds.is_empty() {
            // Wait for a command to be available.
            cmds.push(cmd_rx.recv().ok()?);
        }
    }
}

impl BackgroundJobs {
    pub fn new() -> Self {
        let (cmd_tx, cmd_rx) = crossbeam::channel::unbounded();
        let (diag_tx, diag_rx) = crossbeam::channel::unbounded();

        std::thread::spawn(move || worker(cmd_rx, diag_tx));

        Self {
            receiver: diag_rx,
            sender: cmd_tx,
        }
    }

    pub fn update_file(&mut self, path: PathBuf, text: String) {
        self.sender
            .send(Command::UpdateFile { path, text })
            .unwrap();
    }

    pub fn eval_file(&mut self, path: PathBuf) {
        self.sender.send(Command::EvalFile { path }).unwrap();
    }

    pub fn receiver(&self) -> &Receiver<Response> {
        &self.receiver
    }
}
