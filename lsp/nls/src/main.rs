use std::{fs, io, path::PathBuf};

use anyhow::Result;

use log::debug;
use lsp_server::Connection;

mod actions;
mod analysis;
mod background;
mod cache;
mod codespan_lsp;
mod command;
mod diagnostic;
mod error;
mod field_walker;
mod files;
mod identifier;
mod incomplete;
mod position;
mod requests;
mod server;
use server::Server;
mod pattern;
mod term;
mod trace;
mod usage;
mod utils;
mod world;

use crate::trace::Trace;

#[derive(clap::Parser, Debug)]
/// The LSP server of the Nickel language.
struct Opt {
    /// The trace output file, disables tracing if not given
    #[arg(short, long)]
    trace: Option<PathBuf>,

    /// If set, this process runs a background evaluation job instead of setting up a language server.
    #[arg(long)]
    background_eval: bool,
}

fn main() -> Result<()> {
    use clap::Parser;

    env_logger::init();

    let options = Opt::parse();

    if options.background_eval {
        return background::worker_main();
    }

    if let Some(file) = options.trace {
        debug!("Writing trace to {:?}", file.canonicalize()?);
        Trace::set_writer(csv::Writer::from_writer(io::BufWriter::new(
            fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open(file)?,
        )))?;
    }

    let (connection, _threads) = Connection::stdio();

    let capabilities = Server::capabilities();

    connection.initialize(serde_json::to_value(capabilities)?)?;

    let _server = Server::new(connection).run();

    Ok(())
}
