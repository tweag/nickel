#![cfg(feature = "nix-experimental")]
#[cxx::bridge]
mod internal {
    unsafe extern "C++" {
        include!("nickel-lang-core/src/nix_ffi/cpp/nix.hh");
        fn eval_to_json(nix_code: &str) -> Result<String>;
    }
}

pub use internal::*;
