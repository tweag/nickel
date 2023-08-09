use std::path::PathBuf;

use directories::BaseDirs;
use nickel_lang_core::repl::rustyline_frontend;

use crate::{cli::GlobalOptions, error::CliResult};

#[derive(clap::Parser, Debug)]
pub struct ReplCommand {
    #[arg(long)]
    pub history_file: Option<PathBuf>,
}

impl ReplCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let histfile = if let Some(h) = self.history_file {
            h
        } else {
            BaseDirs::new()
                .expect("Cannot retrieve home directory path")
                .home_dir()
                .join(".nickel_history")
        };
        Ok(rustyline_frontend::repl(histfile, global.color.into())?)
    }
}
