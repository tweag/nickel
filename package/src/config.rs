use std::path::PathBuf;

use directories::ProjectDirs;
use std::collections::HashMap;

const DEFAULT_INDEX_URL: &str = "https://github.com/nickel-lang/nickel-mine.git";

/// Global configuration for the package manager.
#[derive(Clone, Debug)]
pub struct Config {
    pub cache_dir: PathBuf,
    /// The place where we put the downloaded contents of git packages.
    ///
    /// Defaults to `<cache_dir>/git-packages`
    pub git_package_dir: PathBuf,
    /// The place where we put the downloaded package index (but not the contents of
    /// the packages in the index).
    ///
    /// Defaults to `<cache_dir>/index`
    pub index_dir: PathBuf,
    /// The place where we put the downloaded contents of index packages.
    ///
    /// Defaults to `<cache_dir>/index-packages`
    pub index_package_dir: PathBuf,
    /// Git source replacements: any git packages that we're supposed to
    /// fetch from the original source will be transparently fetched from the
    /// replacement source instead. The lock-file will not see this replacement;
    /// it's intended for vendoring or mirroring, not changing the contents of
    /// the package.
    pub git_replacements: HashMap<gix::Url, gix::Url>,
    /// The location to fetch the index from. Currently not configurable (TODO).
    pub index_url: gix::Url,
}

impl Config {
    /// Create a new configuration with default settings.
    pub fn new() -> Result<Self, crate::Error> {
        let cache_dir = ProjectDirs::from("org", "nickel-lang", "nickel")
            .ok_or(crate::Error::NoProjectDir)?
            .cache_dir()
            .to_owned();

        Ok(Self {
            git_package_dir: PathBuf::default(),
            index_dir: PathBuf::default(),
            index_package_dir: PathBuf::default(),
            cache_dir: PathBuf::default(),
            git_replacements: HashMap::default(),
            // unwrap: it's a constant, and we know it's a valid url.
            index_url: DEFAULT_INDEX_URL.try_into().unwrap(),
        }
        .with_cache_dir(cache_dir))
    }

    /// Configures the root cache directory, and reconfigures the various derived paths
    /// based on the new root cache directory.
    pub fn with_cache_dir(self, cache_dir: PathBuf) -> Self {
        Self {
            git_package_dir: cache_dir.join("git-packages"),
            index_dir: cache_dir.join("index"),
            index_package_dir: cache_dir.join("index-packages"),
            cache_dir,
            ..self
        }
    }
}
