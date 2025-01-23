use std::{
    fs::File,
    io::{BufRead as _, BufReader},
    path::{Path, PathBuf},
};

use regex::Regex;
use sha3::{Digest, Sha3_256};

// Turn a hex-encoded string into a byte array, panicking if it isn't properly
// hex-encoded.
fn decode_hex(s: &str) -> Vec<u8> {
    let decode_byte = |b: u8| char::from(b).to_digit(16).unwrap() as u8;
    s.as_bytes()
        .chunks(2)
        .map(|hex| decode_byte(hex[0]) * 16 + decode_byte(hex[1]))
        .collect()
}

// Checks if the grammar in the source tree is up-to-date, by comparing
// the hash of the lalrpop source to the hash that lalrpop recorded in
// the generated file. Lalrpop writes a line like "// sha3: adf234..1234"
// into its generated file, where the hash is the SHA3-256 hash of the
// source file it read.
fn grammar_is_up_to_date(path: &Path) -> bool {
    let grammar_src_path = Path::new(&concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/src/parser/grammar.lalrpop"
    ));
    let reader = BufReader::new(File::open(path).unwrap());
    let sha_regex = Regex::new("sha3: ([a-z0-9]+)").unwrap();
    let mut src_hasher = Sha3_256::new();
    src_hasher.update(std::fs::read_to_string(grammar_src_path).unwrap());
    let src_hash = src_hasher.finalize();

    // The generated file is really big and we don't want to read the whole thing.
    // As of writing this, the "sha3:" line is always the second one. We'll be a
    // little bit robust to changes by looking at the first five lines.
    for line in reader.lines().take(5) {
        if let Some(captures) = sha_regex.captures(&line.unwrap()) {
            let hash = captures.get(1).unwrap();
            let hash = decode_hex(hash.as_str());
            eprintln!("src hash {src_hash:?}, saved hash {hash:?}");
            return hash == src_hash[..];
        }
    }
    false
}

fn main() {
    let checked_in_grammar_path = Path::new(&concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/src/parser/generated/grammar.rs"
    ));

    // Running lalrpop can be expensive, so we check in a generated grammar file.
    // If that file is up to date, copy it into the output directory instead of
    // running lalrpop.
    if grammar_is_up_to_date(checked_in_grammar_path) {
        let out_dir = PathBuf::from(std::env::var_os("OUT_DIR").expect("missing OUT_DIR variable"));
        let out_parser_dir = out_dir.join("parser");
        std::fs::create_dir_all(&out_parser_dir).expect("failed to create $OUT_DIR/parser");
        std::fs::copy(checked_in_grammar_path, out_parser_dir.join("grammar.rs")).unwrap();
    } else {
        lalrpop::Configuration::new()
            .use_cargo_dir_conventions()
            .process_file("src/parser/grammar.lalrpop")
            .unwrap();
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
            .flag_if_supported("-std=c++20")
            .flag_if_supported("-U_FORTIFY_SOURCE") // Otherwise builds with `-O0` raise a lot of warnings
            .compile("nickel-lang");

        println!("cargo:rerun-if-changed=src/nix_ffi/mod.rs");
        println!("cargo:rerun-if-changed=src/nix_ffi/cpp/nix.cc");
        println!("cargo:rerun-if-changed=src/nix_ffi/cpp/nix.hh");
    }
}
