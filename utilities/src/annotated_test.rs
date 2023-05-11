use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};

use serde::Deserialize;

pub struct TestCase<Annot> {
    pub annotation: Annot,
    pub program: String,
}

#[derive(Debug)]
pub enum AnnotatedProgramReadError {
    MissingAnnotation,
}

pub fn read_annotated_test_case<Annot: for<'de> Deserialize<'de>>(
    path: &str,
) -> Result<TestCase<Annot>, AnnotatedProgramReadError> {
    let path = {
        // NOTE: this is a workaround for the lack of a CARGO_WORKSPACE_DIR
        //       environment variable, as suggested [here](https://github.com/rust-lang/cargo/issues/3946#issuecomment-1433384192).
        //       A better workaround might be to set this in the `[env]` section
        //       of `.cargo/config.toml`, as suggested[here](https://github.com/rust-lang/cargo/issues/3946#issuecomment-973132993)
        //       but we currently don't check that file in, and I'm not sure
        //       whether doing so is a good idea.
        let proj_root = concat!(env!("CARGO_MANIFEST_DIR"), "/..");
        PathBuf::from(proj_root).join(path)
    };

    println!("Trying to open {path:?}");
    let file = File::open(path).expect("Failed to open file");
    let reader = BufReader::new(file);

    let mut lines = reader.lines();

    let mut annotation = String::new();
    let mut program = String::new();

    loop {
        let line = lines
            .next()
            .expect("Unexpected end of test file")
            .expect("Error reading line");
        if line.starts_with('#') {
            let annot_line = if line.len() > 1 { &line[2..] } else { "" };
            annotation.push_str(annot_line);
            annotation.push('\n');
        } else {
            // we've already consumed the line in order to check the first char
            // so we need to add it to the program string.
            program.push_str(&line);
            program.push('\n');
            break;
        }
    }

    if annotation.is_empty() {
        return Err(AnnotatedProgramReadError::MissingAnnotation);
    }

    for line in lines {
        let line = line.expect("Error reading line");
        program.push_str(&line);
        program.push('\n');
    }

    let annotation: Annot =
        toml::from_str(annotation.as_str()).expect("Failed to parse expectation toml");

    Ok(TestCase {
        annotation,
        program,
    })
}
