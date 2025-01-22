use std::path::{Path, PathBuf};

fn main() {
    // Lalrpop is slow to generate grammars, so to make things easier for
    // downstream users we check in a generated grammar.rs file. This is a bit
    // awkward when we want to work on the grammar, though, so the build script
    // will autogenerate a new grammar (in $OUT_DIR, so as not to mess up the
    // source tree) whenever the generated grammar is missing from the source
    // tree. Thus, when working on the grammar you just delete
    // `src/parser/generated/grammar.rs` and the build script will rebuild it
    // for you whenever necessary. When you're done working on the grammar,
    // copy the generated file back into `src/parser/generated/grammar.rs`.
    let checked_in_grammar_path = Path::new("src/parser/generated/grammar.rs");
    let out_dir = PathBuf::from(std::env::var_os("OUT_DIR").expect("missing OUT_DIR variable"));
    let out_parser_dir = out_dir.join("parser");
    std::fs::create_dir_all(&out_parser_dir).expect("failed to create $OUT_DIR/parser");

    // If we fail to link the generated grammar into $OUT_DIR, it probably didn't exist.
    // We'll just pretend that's what happened, and try to generate a fresh grammar.
    if std::fs::hard_link(checked_in_grammar_path, out_parser_dir.join("grammar.rs")).is_err() {
        lalrpop::Configuration::new()
            .use_cargo_dir_conventions()
            .process_file("src/parser/grammar.lalrpop")
            .unwrap();
    }

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
            .flag_if_supported("-std=c++20")
            .flag_if_supported("-U_FORTIFY_SOURCE") // Otherwise builds with `-O0` raise a lot of warnings
            .compile("nickel-lang");

        println!("cargo:rerun-if-changed=src/nix_ffi/mod.rs");
        println!("cargo:rerun-if-changed=src/nix_ffi/cpp/nix.cc");
        println!("cargo:rerun-if-changed=src/nix_ffi/cpp/nix.hh");
    }
}
