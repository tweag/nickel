use std::{fs, io, path::PathBuf};

use anyhow::Result;

use log::debug;
use lsp_server::Connection;

mod cache;
mod diagnostic;
mod files;
mod linearization;
mod requests;
mod server;
use server::Server;
use structopt::StructOpt;

use crate::trace::Trace;

mod term;
mod trace;

#[derive(StructOpt, Debug)]
/// The LSP server of the Nickel language.
struct Opt {
    /// The trace output file, disables tracing if not given
    #[structopt(short = "t", long)]
    #[structopt(parse(from_os_str))]
    trace: Option<PathBuf>,
}

fn main() -> Result<()> {
    env_logger::init();

    let options = Opt::from_args();

    if let Some(file) = options.trace {
        debug!("Writing trace to {:?}", file.canonicalize()?);
        Trace::set_writer(csv::Writer::from_writer(io::BufWriter::new(
            fs::OpenOptions::new().append(true).open(file)?,
        )))?;
    }

    let (connection, _threads) = Connection::stdio();

    let capabilities = Server::capabilities();

    connection.initialize(serde_json::to_value(&capabilities)?)?;

    let _server = Server::new(connection).run();

    Ok(())
}
