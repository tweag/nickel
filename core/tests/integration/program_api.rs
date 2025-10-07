use nickel_lang_core::{
    error::Sink,
    eval::cache::lazy::CBNCache,
    program::{Input, Program},
};

// Regression test for https://github.com/tweag/nickel/issues/2362
#[test]
fn multiple_inputs_non_paths() {
    let sink = Sink::default();
    let mut prog: Program<CBNCache> = Program::new_from_inputs(
        [
            Input::Source("{}".to_owned().as_bytes(), &String::from("fst")),
            Input::Source("{} & {}".to_owned().as_bytes(), &String::from("snd")),
        ],
        std::io::stderr(),
        sink,
    )
    .unwrap();

    assert_eq!(&prog.eval_full_for_export().unwrap().to_string(), "{}");
}
