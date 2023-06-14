use anyhow::Result;

use lsp_harness::{LspDebug, Server};
use lsp_types::{Position, Url};

pub fn main() -> Result<()> {
    env_logger::init();
    let cmd = std::process::Command::new("/home/jneeman/tweag/nickel/target/debug/nls");
    let s = Server::new(cmd)?;
    let uri = Url::parse("file:///test.ncl").unwrap();
    s.send_file(uri.clone(), "let var = 5 in { val = var }")?;
    dbg!(s
        .goto_def(
            uri,
            Position {
                line: 0,
                character: 23,
            },
        )?
        .debug_str());
    s.shutdown()?;
    Ok(())
}
