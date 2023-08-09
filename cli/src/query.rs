use nickel_lang_core::repl::query_print;

use crate::{
    cli::GlobalOptions,
    error::{CliResult, ResultErrorExt, Warning},
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
    fn attributes_specified(&self) -> bool {
        self.doc || self.contract || self.typ || self.default || self.value
    }

    fn query_attributes(&self) -> query_print::Attributes {
        // Use a default selection of attributes if no option is specified
        if !self.attributes_specified() {
            query_print::Attributes::default()
        } else {
            query_print::Attributes {
                doc: self.doc,
                contract: self.contract,
                typ: self.typ,
                default: self.default,
                value: self.value,
            }
        }
    }

    pub fn run(mut self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.evaluation.prepare(&global)?;

        if self.path.is_none() {
            program.report(Warning::EmptyQueryPath)
        }

        program
            .query(std::mem::take(&mut self.path))
            .map(|field| {
                query_print::write_query_result(
                    &mut std::io::stdout(),
                    &field,
                    self.query_attributes(),
                )
                .unwrap()
            })
            .report_with_program(program)
    }
}
