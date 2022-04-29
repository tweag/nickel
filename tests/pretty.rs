use nickel_lang::pretty::*;
use nickel_lang::term::Term;
use nickel_lang_utilities::parse;
use pretty::BoxAllocator;
use std::ffi::OsString;
use std::fs;
use std::io::{Cursor, Read, Write};
use std::path::PathBuf;

fn check_file(file: &str) {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(format!("tests/pass/{}", file));

    let mut buffer = String::new();
    fs::File::open(&path)
        .and_then(|mut file| file.read_to_string(&mut buffer))
        .unwrap();

    let rt = parse(&buffer).unwrap().without_pos();

    let allocator = BoxAllocator;
    let mut rt_pretty = Cursor::new(Vec::new());

    let doc: DocBuilder<_, ()> = rt.clone().pretty(&allocator);
    doc.render(80, &mut rt_pretty).unwrap();
    assert_eq!(
        rt,
        parse(String::from_utf8_lossy(rt_pretty.get_ref()).as_ref())
            .unwrap()
            .without_pos()
    );
}

#[test]
fn basics() {
    check_file("basics.ncl");
}
