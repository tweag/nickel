use crate::{customize::NoCustomizeMode, global::GlobalContext, input::InputOptions};

#[derive(clap::Parser, Debug)]
pub struct PprintAstCommand {
    /// Performs code transformations before printing
    #[arg(long)]
    pub transform: bool,

    #[command(flatten)]
    pub inputs: InputOptions<NoCustomizeMode>,
}

impl PprintAstCommand {
    pub fn run(self, ctxt: &mut GlobalContext) {
        ctxt.with_program(&self.inputs, |program| {
            program.pprint_ast(&mut std::io::stdout(), self.transform)
        });
    }
}
