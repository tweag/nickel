fn main() {
    #[cfg(feature = "nix-experimental")]
    {
        use cxx_build::CFG;
        use std::path::PathBuf;

        for lib in &["nix-store", "nix-cmd", "nix-expr", "nix-main"] {
            let lib = pkg_config::Config::new()
                .atleast_version("2.16.0")
                .probe(lib)
                .unwrap();
            let lib_include_paths = lib.include_paths.iter().map(PathBuf::as_path);
            CFG.exported_header_dirs.extend(lib_include_paths);
        }

        cxx_build::bridge("src/nix_ffi/mod.rs")
            .file("src/nix_ffi/cpp/nix.cc")
            .std("c++20")
            .cpp(true)
            .flag_if_supported("-U_FORTIFY_SOURCE") // Otherwise builds with `-O0` raise a lot of warnings
            .compile("nickel-lang");

        println!("cargo:rerun-if-changed=src/nix_ffi/mod.rs");
        println!("cargo:rerun-if-changed=src/nix_ffi/cpp/nix.cc");
        println!("cargo:rerun-if-changed=src/nix_ffi/cpp/nix.hh");
    }
}
