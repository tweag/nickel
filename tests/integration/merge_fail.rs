use assert_matches::assert_matches;
use nickel_lang::error::{Error, EvalError};
use nickel_lang::position::TermPos;
use nickel_lang::term::RichTerm;
use nickel_lang_utilities::TestProgram;
use std::io::Cursor;

fn eval_full(s: &str) -> Result<RichTerm, Error> {
    let src = Cursor::new(s);

    let mut p = TestProgram::new_from_source(src, "<test>").map_err(|io_err| {
        Error::EvalError(EvalError::Other(
            format!("IO error: {}", io_err),
            TermPos::None,
        ))
    })?;
    p.eval_full()
}

macro_rules! assert_merge_fails {
    ($term:expr) => {
        assert_matches!(
            eval_full($term),
            Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
        )
    };
}

#[test]
fn merge_conflict_inside_metavalue() {
    assert_merge_fails!(
        "{ foo | default = (fun x => x) 1, foo | default = (fun x => x) 1} & {foo | default = 2 }"
    );
}
