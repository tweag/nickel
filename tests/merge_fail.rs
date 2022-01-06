use assert_matches::assert_matches;
use nickel::error::{Error, EvalError};
use nickel::position::TermPos;
use nickel::program::Program;
use nickel::term::RichTerm;
use std::io::Cursor;

fn eval_full(s: &str) -> Result<RichTerm, Error> {
    let src = Cursor::new(s);

    let mut p = Program::new_from_source(src, "<test>").map_err(|io_err| {
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
    assert_merge_fails!("{ foo = (fun x => x) (1 | default), foo = (fun x => x) (1 | default) } & {foo | default = 2 }");
}
