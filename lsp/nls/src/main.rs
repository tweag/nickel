use anyhow::Result;
use log::trace;
use lsp_server::Connection;

mod cache;
mod diagnostic;
mod files;
mod linearization;
mod requests;
mod server;
use server::Server;

mod term;
mod trace;

fn main() -> Result<()> {
    env_logger::init();

    trace!("hello");

    let (connection, _threads) = Connection::stdio();

    let capabilities = Server::capabilities();

    connection.initialize(serde_json::to_value(&capabilities)?)?;

    let _server = Server::new(connection).run();

    Ok(())
}
