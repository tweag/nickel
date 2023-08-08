use nickel_lang_core::repl::query_print;

use crate::{
    cli::GlobalOptions,
    error::{CliResult, ResultErrorExt},
    eval::EvalCommand,
};

#[derive(clap::Parser, Debug)]
pub struct QueryCommand {
    pub path: Option<String>,

    #[arg(long)]
    pub doc: bool,

    #[arg(long)]
    pub contract: bool,

    #[arg(long = "type")]
    pub typ: bool,

    #[arg(long)]
    pub default: bool,

    #[arg(long)]
    pub value: bool,

    #[command(flatten)]
    pub evaluation: EvalCommand,
}

impl QueryCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.evaluation.prepare(&global)?;

        program
            .query(self.path)
            .map(|term| {
                // Print a default selection of attributes if no option is specified
                let attrs =
                    if !self.doc && !self.contract && !self.typ && !self.default && !self.value {
                        query_print::Attributes::default()
                    } else {
                        query_print::Attributes {
                            doc: self.doc,
                            contract: self.contract,
                            typ: self.typ,
                            default: self.default,
                            value: self.value,
                        }
                    };

                query_print::write_query_result(&mut std::io::stdout(), &term, attrs).unwrap()
            })
            .report_with_program(program)
    }
}
