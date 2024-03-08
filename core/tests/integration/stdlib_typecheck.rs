use assert_matches::assert_matches;
use nickel_lang_core::{cache_new::SourceCache, driver};

#[test]
fn stdlib_typecheck() {
    let mut cache = SourceCache::new();
    driver::load_stdlib(&mut cache);
    assert_matches!(driver::typecheck_stdlib(&mut cache), Ok(_));
}
