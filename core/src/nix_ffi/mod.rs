use cxx::let_cxx_string;

#[cxx::bridge]
mod internal {
    unsafe extern "C++" {
        include!("nickel-lang-core/src/nix_ffi/cpp/nix.hh");
        fn eval_to_json(nix_code: &str, base_dir: &CxxString) -> Result<String>;
    }
}

/// Calls to the Nix FFI to evaluate the given Nix code and returns the result as a JSON string.
///
/// # Arguments
///
/// - `nix_code`: the Nix code to evaluate, as a string.
/// - `base_dir`: the base directory passed to the Nix evaluator, which is used to resolve imports
///   on the Nix side.
pub fn eval_to_json(nix_code: &str, base_dir: &std::path::Path) -> Result<String, cxx::Exception> {
    // There is no builtin binding type for passing `OsStr` across FFI boundaries in cxx currently
    // (https://github.com/dtolnay/cxx/issues/1291), so we build a raw [u8] slice, then a
    // `std::string` from it (or rather the corresponding version `CxxString` in the Rust world)
    // and pass this to the C++ side.
    let path_bytes = base_dir.as_os_str().as_encoded_bytes();
    let_cxx_string!(path_string = path_bytes);
    internal::eval_to_json(nix_code, &path_string)
}
