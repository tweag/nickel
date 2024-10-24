use nickel_lang_core::typecheck::TypecheckMode;

use crate::{
    cli::GlobalOptions,
    customize::NoCustomizeMode,
    error::{CliResult, ResultErrorExt},
    input::{InputOptions, Prepare},
};

#[derive(clap::Parser, Debug)]
pub struct TypecheckCommand {
    #[command(flatten)]
    inputs: InputOptions<NoCustomizeMode>,

    /// Start the type-checker in strict mode, so that the entire input is treated as a typed block.
    #[arg(long, global = true)]
    pub strict_typechecking: bool,
}

impl TypecheckCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        if self.strict_typechecking {
            // In strict mode we run *both* forms of typechecking, because in
            // fact neither one is more strict than the other. For example,
            // given the input
            //
            // let x = (1 + 1) in (x + 1 : Number)
            //
            // typechecking in "enforce" mode will succeed because it will infer
            // `x: Number`, while typechecking in walk mode will fail because it
            // will treat `x` as `Dyn` and then try to typecheck `x + 1`.
            let mut program = self.inputs.prepare(&global)?;
            program
                .typecheck(TypecheckMode::Enforce)
                .report_with_program(program)?;
        }
        let mut program = self.inputs.prepare(&global)?;
        program
            .typecheck(TypecheckMode::Walk)
            .report_with_program(program)
    }
}
