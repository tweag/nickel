use nickel_lang_core::typecheck::TypecheckMode;

use crate::{customize::NoCustomizeMode, global::GlobalContext, input::InputOptions};

#[derive(clap::Parser, Debug)]
pub struct TypecheckCommand {
    #[command(flatten)]
    inputs: InputOptions<NoCustomizeMode>,

    /// Start the type-checker in strict mode, so that the entire input is treated as a typed block.
    #[arg(long, global = true)]
    pub strict_typechecking: bool,
}

impl TypecheckCommand {
    pub fn run(self, ctxt: &mut GlobalContext) {
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
            ctxt.with_program(&self.inputs, |prog| prog.typecheck(TypecheckMode::Enforce));
        }
        ctxt.with_program(&self.inputs, |prog| prog.typecheck(TypecheckMode::Walk));
    }
}
