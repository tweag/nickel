use std::path::PathBuf;

use directories::BaseDirs;
use nickel_lang_core::repl::rustyline_frontend;

use crate::{cli::GlobalOptions, color_opt_from_clap, error::CliResult};

#[derive(clap::Parser, Debug)]
pub struct ReplCommand {
    #[arg(long)]
    pub history_file: Option<PathBuf>,
}

impl ReplCommand {
    pub fn run(self, ctxt: &mut GlobalContext) {
        let histfile = if let Some(h) = self.history_file {
            h
        } else {
            BaseDirs::new()
                .expect("Cannot retrieve home directory path")
                .home_dir()
                .join(".nickel_history")
        };
        ctxt.report_result(rustyline_frontend::repl(
            histfile,
            color_opt_from_clap(ctxt.opts.color),
        ));
    }
}
