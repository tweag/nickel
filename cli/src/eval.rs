use crate::{
    customize::CustomizeMode,
    global::GlobalContext,
    input::{InputOptions, StdinFormat},
};

#[derive(clap::Parser, Debug)]
pub struct EvalCommand {
    #[command(flatten)]
    pub input: InputOptions<CustomizeMode, StdinFormat>,
}

impl EvalCommand {
    pub fn run(self, ctxt: &mut GlobalContext) {
        ctxt.with_program(&self.input, |program| {
            program.eval_full().map(|t| println!("{t}"))
        });
    }
}
