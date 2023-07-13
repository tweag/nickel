use std::{path::PathBuf, process};

use directories::BaseDirs;
use nickel_lang_core::{program::ColorOpt, repl::rustyline_frontend};

pub fn repl(history_file: Option<PathBuf>, color: ColorOpt) {
    let histfile = if let Some(h) = history_file {
        h
    } else {
        BaseDirs::new()
            .expect("Cannot retrieve home directory path")
            .home_dir()
            .join(".nickel_history")
    };
    if rustyline_frontend::repl(histfile, color).is_err() {
        process::exit(1);
    }
}
