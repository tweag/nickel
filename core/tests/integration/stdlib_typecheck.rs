use assert_matches::assert_matches;
use nickel_lang_core::{cache_new::SourceCache, prepare};

#[test]
fn stdlib_typecheck() {
    let mut cache = SourceCache::new();
    prepare::load_stdlib(&mut cache);
    assert_matches!(prepare::typecheck_stdlib(&mut cache), Ok(_));
}
