// XXX: this makes it nix_ffi::nix_ffi
#[cxx::bridge]
pub mod nix_ffi {
    unsafe extern "C++" {
        include!("nickel-lang-core/src/nix_ffi/cpp/nix.hh");
        fn eval_to_json(nix_code: &str) -> String;
    }
}
