use std::path::{Path, PathBuf};

fn main() {
    let checked_in_grammar_path = Path::new(&concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/src/parser/grammar.rs"
    ));

    let out_dir = PathBuf::from(std::env::var_os("OUT_DIR").expect("missing OUT_DIR variable"));
    let out_parser_dir = out_dir.join("parser");
    std::fs::create_dir_all(&out_parser_dir).expect("failed to create $OUT_DIR/parser");
    // Running lalrpop can be expensive. When building from git, we generate the parser
    // in this build script, but when publishing the crate we add the generated
    // parser to the published crate at `src/parser/grammar.rs`.
    //
    // In order to have this build script work for both the published crate and the git
    // version, we try to copy `src/parser/grammar.rs` into the same location in $OUT_DIR
    // that lalrpop would generate the grammar. If that copy fails because `src/parser/grammar.rs`
    // doesn't exist, we're probably building from git and so we generate the grammar.
    match std::fs::copy(checked_in_grammar_path, out_parser_dir.join("grammar.rs")) {
        Ok(_) => {
            eprintln!("Found a pre-generated LALRPOP grammar, copying it over");
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            eprintln!("Generating a fresh LALRPOP grammar");
            lalrpop::Configuration::new()
                .use_cargo_dir_conventions()
                .process_file("src/parser/grammar.lalrpop")
                .unwrap();
        }
        Err(e) => panic!("{e}"),
    }
    println!("cargo:rerun-if-changed=src/parser/grammar.lalrpop");

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
            .std("c++23")
            .cpp(true)
            .flag_if_supported("-U_FORTIFY_SOURCE") // Otherwise builds with `-O0` raise a lot of warnings
            .compile("nickel-lang");

        println!("cargo:rerun-if-changed=src/nix_ffi/mod.rs");
        println!("cargo:rerun-if-changed=src/nix_ffi/cpp/nix.cc");
        println!("cargo:rerun-if-changed=src/nix_ffi/cpp/nix.hh");
    }
}
