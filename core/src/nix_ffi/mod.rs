#[cxx::bridge]
mod nix_ffi {
    unsafe extern "C++" {
        include!("nickel-lang-core/src/nix_ffi/cpp/nix.hh");
        fn eval_to_json(nix_code: &str) -> Result<String>;
    }
}

pub use nix_ffi::*;
