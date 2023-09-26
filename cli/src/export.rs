use std::{fs, io::Write, path::PathBuf};

use nickel_lang_core::{
    error::{Error, IOError},
    eval::cache::lazy::CBNCache,
    program::{FieldOverride, Program},
    serialize::{self, ExportFormat},
};

use crate::{
    cli::GlobalOptions,
    customize::CustomizeMode,
    error::{CliResult, ResultErrorExt},
    eval::EvalCommand,
};

#[derive(clap::Parser, Debug)]
pub struct ExportCommand {
    #[arg(long, value_enum, default_value_t)]
    pub format: ExportFormat,

    /// Output file. Standard output by default
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    #[command(flatten)]
    pub evaluation: EvalCommand,

    #[command(flatten)]
    pub customize_mode: CustomizeMode,
}

impl ExportCommand {
    pub fn run(mut self, global: GlobalOptions) -> CliResult<()> {
        let program = self.evaluation.prepare(&global)?;
        let (mut program, overrides) = self.customize_mode.get_overrides(program)?;

        self.export(&mut program, overrides)
            .report_with_program(program)
    }

    fn export(
        self,
        program: &mut Program<CBNCache>,
        overrides: Vec<FieldOverride>,
    ) -> Result<(), Error> {
        let rt = program.eval_full_for_export(overrides)?;

        // We only add a trailing newline for JSON exports. Both YAML and TOML
        // exporters already append a trailing newline by default.
        let trailing_newline = self.format == ExportFormat::Json;

        serialize::validate(self.format, &rt)?;

        if let Some(file) = self.output {
            let mut file = fs::File::create(file).map_err(IOError::from)?;
            serialize::to_writer(&mut file, self.format, &rt)?;

            if trailing_newline {
                writeln!(file).map_err(IOError::from)?;
            }
        } else {
            serialize::to_writer(std::io::stdout(), self.format, &rt)?;

            if trailing_newline {
                println!();
            }
        }

        Ok(())
    }
}
