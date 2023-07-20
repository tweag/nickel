fn main() {
    lalrpop::Configuration::new()
        .use_cargo_dir_conventions()
        .process_file("src/parser/grammar.lalrpop")
        .unwrap();

    #[cfg(feature = "nix-experimental")]
    cxx_build::bridge("src/nix_ffi/mod.rs")
        .file("src/nix_ffi/cpp/nix.cc")
        .flag_if_supported("-std=c++17")
        .flag_if_supported("-U_FORTIFY_SOURCE") // Otherwise builds with `-O0` raise a lot of warnings
        .compile("nickel-lang");
    println!("cargo:rustc-link-lib=nixstore");
    println!("cargo:rustc-link-lib=nixcmd");
    println!("cargo:rustc-link-lib=nixexpr");
    println!("cargo:rustc-link-lib=nixmain");
    println!("cargo:rustc-link-lib=nixutil");

    println!("cargo:rerun-if-changed=src/nix_ffi/mod.rs");
    println!("cargo:rerun-if-changed=src/nix_ffi/cpp/nix.cc");
    println!("cargo:rerun-if-changed=src/nix_ffi/cpp/nix.hh");
}
