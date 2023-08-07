use crate::{
    cli::{GlobalOptions, Options},
    error::CliResult,
};

#[derive(clap::Parser, Debug)]
pub struct GenCompletionsCommand {
    #[arg(value_enum)]
    pub shell: clap_complete::Shell,
}

impl GenCompletionsCommand {
    pub fn run(self, _: GlobalOptions) -> CliResult<()> {
        clap_complete::generate(
            self.shell,
            &mut <Options as clap::CommandFactory>::command(),
            env!("CARGO_BIN_NAME"),
            &mut std::io::stdout(),
        );
        Ok(())
    }
}
