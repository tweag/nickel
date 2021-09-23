use anyhow::Result;
use log::trace;
use lsp_server::Connection;
use nickel::language_server::server::Server;

fn main() -> Result<()> {
    env_logger::init();

    trace!("hello");

    let (connection, threads) = Connection::stdio();

    let capabilities = Server::capabilities();

    connection.initialize(serde_json::to_value(&capabilities)?)?;

    let server = Server::new(connection).run();

    Ok(())
}
