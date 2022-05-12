use nickel_lang::pretty::*;
use nickel_lang::term::RichTerm;
use nickel_lang_utilities::parse;
use pretty::BoxAllocator;
use std::borrow::Borrow;
use std::io::{Cursor, Read, Write};
use std::path::PathBuf;

fn diff(s1: &str, s2: &str) {
    use similar::*;
    let diff = TextDiff::from_lines(s1, s2);
    let mut nb_diff = 0;

    for change in diff.iter_all_changes() {
        match change.tag() {
            ChangeTag::Delete => {
                nb_diff += 1;
                println!("- {}", change);
            }
            ChangeTag::Insert => {
                nb_diff += 1;
                println!("+ {}", change);
            }
            ChangeTag::Equal => (),
        };
    }
    assert!(nb_diff == 0);
}

fn pretty(rt: &RichTerm) -> String {
    let allocator = BoxAllocator;
    let mut ret = Vec::new();
    let mut rt_pretty = Cursor::new(&mut ret);

    let doc: DocBuilder<_, ()> = rt.clone().pretty(&allocator);
    doc.render(80, &mut rt_pretty).unwrap();
    String::from_utf8_lossy(&ret).into_owned()
}

fn check_file(file: &str) {
    let mut buffer = String::new();
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(format!("tests/pass/{}", file));
    std::fs::File::open(&path)
        .and_then(|mut file| file.read_to_string(&mut buffer))
        .unwrap();

    let rt = parse(&buffer).unwrap();
    let pretty_rt = pretty(&rt);
    let double_pretty = pretty(&parse(&pretty_rt).unwrap());

    diff(&pretty_rt, &double_pretty);
}

#[test]
fn basics() {
    check_file("basics.ncl");
}

#[test]
fn builtins() {
    check_file("builtins.ncl");
}

#[test]
fn complete() {
    check_file("complete.ncl");
}

#[test]
fn eq() {
    check_file("eq.ncl")
}

#[test]
fn functions() {
    check_file("functions.ncl");
}

#[test]
fn arrays() {
    check_file("arrays.ncl");
}
// TODO: Maybe fix the issue with transformation of `let A = Num` form
// in `let A = $num` which is not parsable.
//#[test]
//fn metavalues() {
//    check_file("metavalues.ncl");
//}
//
//#[test]
//fn contracts() {
//    check_file("contracts.ncl");
//}

#[test]
fn records() {
    check_file("records.ncl");
}

#[test]
fn record_defs() {
    check_file("record-defs.ncl");
}

#[test]
fn strings() {
    check_file("strings.ncl");
}

#[test]
fn typechecking() {
    check_file("typechecking.ncl");
}

#[test]
fn types() {
    check_file("types.ncl");
}

#[test]
fn serialize() {
    check_file("serialize.ncl");
    check_file("serialize-package.ncl");
}

#[test]
fn annot_parsing() {
    check_file("annotations.ncl");
}

#[test]
fn importing() {
    check_file("import.ncl");
}

#[test]
fn overriding() {
    check_file("overriding.ncl");
}
