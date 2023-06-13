use std::path::PathBuf;

/// NOTE: this is a workaround for the lack of a CARGO_WORKSPACE_DIR
///       environment variable, as suggested [here](https://github.com/rust-lang/cargo/issues/3946#issuecomment-1433384192).
///       A better workaround might be to set this in the `[env]` section
///       of `.cargo/config.toml`, as suggested[here](https://github.com/rust-lang/cargo/issues/3946#issuecomment-973132993)
///       but we currently don't check that file in, and I'm not sure
///       whether doing so is a good idea.
pub fn project_root() -> PathBuf {
    PathBuf::from(&concat!(env!("CARGO_MANIFEST_DIR"), "/.."))
}
