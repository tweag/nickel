use assert_matches::assert_matches;
use nickel_lang::cache::Cache;

#[test]
fn stdlib_typecheck() {
    let mut cache = Cache::new();
    assert_matches!(cache.load_stdlib(), Ok(_));
    assert_matches!(cache.typecheck_stdlib(), Ok(_));
}
