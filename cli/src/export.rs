use std::{fs, path::PathBuf};

use nickel_lang_core::{
    error::{Error, IOError},
    eval::{cache::lazy::CBNCache, value::NickelValue},
    program::Program,
    serialize::{self, ExportFormat},
};

use crate::{
    customize::CustomizeMode,
    global::GlobalContext,
    input::{InputOptions, StdinFormat},
};

#[derive(clap::Parser, Debug)]
pub struct ExportCommand {
    #[arg(long, short, value_enum, default_value_t)]
    pub format: ExportFormat,

    /// Output file. Standard output by default
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    #[command(flatten)]
    pub input: InputOptions<CustomizeMode, StdinFormat>,
}

impl ExportCommand {
    pub fn run(self, ctxt: &mut GlobalContext) {
        ctxt.with_program(&self.input, |program| self.export(program));
    }

    fn export(&self, program: &mut Program<CBNCache>) -> Result<(), Error> {
        let rt = program.eval_full_for_export()?;

        serialize::validate(self.format, &rt)
            .map_err(|error| error.with_pos_table(program.pos_table().clone()))?;

        if let Some(file) = &self.output {
            let out = std::io::BufWriter::new(fs::File::create(file).map_err(IOError::from)?);
            self.export_to(program, &rt, out)?;
        } else {
            let out = std::io::BufWriter::new(std::io::stdout().lock());
            self.export_to(program, &rt, out)?;
        }

        Ok(())
    }

    fn export_to(
        &self,
        program: &mut Program<CBNCache>,
        value: &NickelValue,
        mut out: impl std::io::Write,
    ) -> Result<(), Error> {
        // We only add a trailing newline for JSON exports. Both YAML and TOML
        // exporters already append a trailing newline by default.
        let trailing_newline = self.format == ExportFormat::Json;

        serialize::to_writer(&mut out, self.format, value)
            .map_err(|error| error.with_pos_table(program.pos_table().clone()))?;

        if trailing_newline {
            writeln!(out).map_err(IOError::from)?;
        }
        out.flush().map_err(IOError::from)?;
        Ok(())
    }
}
