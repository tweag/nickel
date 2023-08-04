use crate::cli::{GlobalOptions, Options};

#[derive(clap::Parser, Debug)]
pub struct GenCompletionsOptions {
    #[arg(value_enum)]
    pub shell: clap_complete::Shell,
}

impl GenCompletionsOptions {
    pub fn run(self, _: GlobalOptions) {
        clap_complete::generate(
            self.shell,
            &mut <Options as clap::CommandFactory>::command(),
            env!("CARGO_BIN_NAME"),
            &mut std::io::stdout(),
        )
    }
}
