use assert_matches::assert_matches;
use colorchoice::ColorChoice;
use nickel_lang_core::{
    cache::{Caches, CacheError, ErrorTolerance},
    error::report::ErrorFormat,
};

#[test]
fn stdlib_typecheck() {
    let mut cache = Caches::new();
    assert_matches!(cache.load_stdlib(), Ok(_));

    if let Err(e) = cache.typecheck_stdlib() {
        let CacheError::Error(e) = e else {
            panic!("we just parsed it");
        };

        nickel_lang_core::error::report::report(
            &mut cache.files().clone(),
            e,
            ErrorFormat::Text,
            ColorChoice::Auto,
        );
        panic!();
    }
}
