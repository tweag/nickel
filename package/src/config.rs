use std::path::PathBuf;

use directories::ProjectDirs;
use std::collections::HashMap;

const DEFAULT_INDEX_URL: &str = "https://github.com/nickel-lang/nickel-mine.git";

/// Global configuration for the package manager.
#[derive(Clone, Debug)]
pub struct Config {
    pub index_url: gix::Url,

    pub cache_dir: PathBuf,

    /// Defaults to `<cache_dir>/index`
    pub index_dir: PathBuf,
    /// Defaults to `<cache_dir>/index-packages`
    pub index_package_dir: PathBuf,
    /// Defaults to `<cache_dir>/git-packages`
    pub git_package_dir: PathBuf,

    /// Git source replacements: any git packages that we're supposed to
    /// fetch from the original source will be transparently fetched from the
    /// replacement source instead. The lock-file will not see this replacement;
    /// it's intended for vendoring or mirroring, not changing the contents of
    /// the package.
    pub git_replacements: HashMap<gix::Url, gix::Url>,
    // TODO: index replacments (and private indices)
}

impl Default for Config {
    fn default() -> Self {
        // unwrap: TODO
        let cache_dir = ProjectDirs::from("org", "nickel-lang", "nickel")
            .unwrap()
            .cache_dir()
            .to_owned();
        Self {
            // unwrap: it's a constant, and we know it's a valid url.
            index_url: DEFAULT_INDEX_URL.try_into().unwrap(),
            index_dir: PathBuf::default(),
            index_package_dir: PathBuf::default(),
            git_package_dir: PathBuf::default(),
            cache_dir: PathBuf::default(),
            git_replacements: HashMap::default(),
        }
        .with_cache_dir(cache_dir)
    }
}

impl Config {
    /// Configures the root cache directory, and reconfigures the various derived paths
    /// based on the new root cache directory.
    pub fn with_cache_dir(self, cache_dir: PathBuf) -> Self {
        Self {
            index_dir: cache_dir.join("index"),
            index_package_dir: cache_dir.join("index-packages"),
            git_package_dir: cache_dir.join("git-packages"),
            ..self
        }
    }

    pub fn with_index_dir(self, index_dir: PathBuf) -> Self {
        Self { index_dir, ..self }
    }
}
