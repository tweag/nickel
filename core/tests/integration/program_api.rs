use nickel_lang_core::{
    cache::InputFormat,
    error::Sink,
    eval::cache::lazy::CBNCache,
    program::{Program, ProgramBuilder},
    typecheck::TypecheckMode,
};

// Regression test for https://github.com/tweag/nickel/issues/2362
#[test]
fn multiple_inputs_non_paths() {
    let mut prog: Program<CBNCache> = ProgramBuilder::new()
        .add_source_string("{}", "fst")
        .add_source_string("{} & {}", "snd")
        .with_trace(std::io::stderr())
        .with_reporter(Sink::default())
        .build()
        .unwrap();

    assert_eq!(&prog.eval_full_for_export().unwrap().to_string(), "{}");
}

// Regression test for https://github.com/nickel-lang/nickel/issues/2670
#[test]
fn typecheck_non_nickel_main_input() {
    let mut prog: Program<CBNCache> = ProgramBuilder::new()
        .add_source_with_format("foo: 1\n".as_bytes(), "foo.yaml", InputFormat::Yaml)
        .with_trace(std::io::stderr())
        .with_reporter(Sink::default())
        .build()
        .unwrap();

    assert!(prog.typecheck(TypecheckMode::Walk).is_ok());
}
