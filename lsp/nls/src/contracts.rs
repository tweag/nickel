//! Configuration for the Nickel Language Server
use lsp_types::Url;
use notify_debouncer_full::{
    DebouncedEvent, Debouncer, RecommendedCache,
    notify::{EventKind, RecommendedWatcher, RecursiveMode},
};
use serde::Deserialize;

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::{Arc, RwLock},
    time::Duration,
};

use crate::files::uri_to_path;

pub const CONFIG_FILE_NAME: &str = "Nls-contracts.ncl";

/// Contract configuration for nls.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct ContractConfig {
    /// Which set of files should this contract be applied to?
    pub glob: glob::Pattern,
    pub contract_path: PathBuf,
}

/// Contract configuration for nls.
#[derive(Debug, Deserialize)]
pub struct ContractConfigFormat {
    /// Which set of files should this contract be applied to?
    glob: String,
    /// This can be a relative or absolute path. If relative, it's relative to
    /// the local config file that this belongs to.
    contract_path: PathBuf,
}

impl ContractConfigFormat {
    fn relative_to(self, parent_path: &Path) -> Result<ContractConfig, glob::PatternError> {
        Ok(ContractConfig {
            glob: self.glob.parse()?,
            contract_path: parent_path.join(self.contract_path),
        })
    }
}

/// A collection of file-local nls configurations, provided by a single
/// config file.
///
/// When we find a config file (named [`CONFIG_FILE_NAME`]) in the filesystem,
/// we look inside for a "glob -> config" mapping, and apply the
/// provided config to every file matching the glob.
#[derive(Debug, Default, PartialEq)]
pub struct LocalConfig {
    globs: Vec<ContractConfig>,
}

/// The serialization format for [`LocalConfig`].
#[derive(Debug, Deserialize)]
#[serde(transparent)]
struct LocalConfigFormat {
    globs: Vec<ContractConfigFormat>,
}

impl LocalConfigFormat {
    fn into_local_config(self, parent_path: &Path) -> anyhow::Result<LocalConfig> {
        Ok(LocalConfig {
            globs: self
                .globs
                .into_iter()
                .map(|cfg| cfg.relative_to(parent_path))
                .collect::<Result<_, glob::PatternError>>()?,
        })
    }
}

impl LocalConfig {
    fn from_file(path: &Path) -> Result<Self, anyhow::Error> {
        let local_config: LocalConfigFormat = nickel_lang_core::deserialize::from_path(path)
            // Eagerly format the error, because anyhow requires `Send` errors and ours aren't
            .map_err(|e| anyhow::anyhow!("{}", e.to_string()))?;
        let parent_path = path
            .parent()
            .ok_or_else(|| anyhow::anyhow!("local config {} has no parent", path.display()))?;
        local_config.into_local_config(parent_path)
    }
}

pub struct LocalConfigs {
    configs: HashMap<PathBuf, LocalConfig>,
}

/// A self-updating collection of [`LocalConfig`]s.
pub struct ContractConfigsWatcher {
    configs: Arc<RwLock<LocalConfigs>>,
    // The collection of directories where we're already watching
    // for a config file.
    already_watched: HashSet<PathBuf>,
    debouncer: Option<Debouncer<RecommendedWatcher, RecommendedCache>>,
}

impl LocalConfigs {
    pub fn config_for(&self, uri: &Url) -> Option<ContractConfig> {
        let path = uri_to_path(uri).ok()?;
        let mut cur = path.as_path();
        while let Some(parent) = cur.parent() {
            if let Some(nls_conf) = self.configs.get(parent) {
                for config in &nls_conf.globs {
                    if let Ok(relative) = path.strip_prefix(parent) {
                        if config.glob.matches_path(relative) {
                            return Some(config.clone());
                        }
                    }
                }
            }
            cur = parent;
        }
        None
    }

    pub fn insert(&mut self, path: PathBuf, config: LocalConfig) {
        self.configs.insert(path, config);
    }

    pub fn remove(&mut self, path: &Path) {
        self.configs.remove(path);
    }
}

impl ContractConfigsWatcher {
    pub fn new() -> Self {
        let configs = Arc::new(RwLock::new(LocalConfigs {
            configs: HashMap::new(),
        }));
        let configs_clone = Arc::clone(&configs);
        let result = notify_debouncer_full::new_debouncer(
            Duration::from_millis(500),
            None,
            move |event: Result<Vec<DebouncedEvent>, _>| match event {
                Ok(evs) => {
                    for ev in evs {
                        let Some(path) = ev
                            .event
                            .paths
                            .iter()
                            .find(|p| p.file_name() == Some(CONFIG_FILE_NAME.as_ref()))
                        else {
                            continue;
                        };
                        match ev.event.kind {
                            EventKind::Create(_) | EventKind::Modify(_) => {
                                Self::load_config(&configs_clone, path);
                            }
                            EventKind::Remove(_) => {
                                if let Some(parent) = path.parent() {
                                    configs_clone.write().unwrap().remove(parent);
                                    // There's no guarantee about the order
                                    // of debounced events: it could have been
                                    // removed and then replaced.
                                    Self::load_config(&configs_clone, path);
                                }
                            }
                            _ => {}
                        }
                    }
                }
                Err(es) => {
                    for e in es {
                        log::warn!("while watching for config changes: {e}");
                    }
                }
            },
        );

        let debouncer = match result {
            Ok(debouncer) => Some(debouncer),
            Err(e) => {
                log::warn!("failed to initialize file watcher, ignoring local config: {e}");
                None
            }
        };

        Self {
            configs,
            debouncer,
            already_watched: HashSet::new(),
        }
    }

    pub fn dummy() -> Self {
        Self {
            configs: Arc::new(RwLock::new(LocalConfigs {
                configs: HashMap::new(),
            })),
            debouncer: None,

            already_watched: HashSet::new(),
        }
    }

    fn load_config(configs: &Arc<RwLock<LocalConfigs>>, path: &Path) {
        match LocalConfig::from_file(path) {
            Ok(conf) => {
                log::info!("reloaded local config {conf:?} for path {}", path.display());
                if let Some(parent) = path.parent() {
                    configs.write().unwrap().insert(parent.to_owned(), conf);
                }
            }
            Err(e) => log::warn!("failed to read config: {e}"),
        }
    }

    pub fn watch_configs_for(&mut self, uri: &Url) {
        if let Some(debouncer) = self.debouncer.as_mut() {
            if let Ok(path) = uri_to_path(uri) {
                let mut cur = path.as_path();
                while let Some(parent) = cur.parent() {
                    if !self.already_watched.insert(parent.to_owned()) {
                        // If we're already watching a directory then we're
                        // also already watching the parent directories,
                        // so just stop early.
                        break;
                    }

                    log::debug!("adding watcher for {}", parent.display());
                    if let Err(e) = debouncer.watch(parent, RecursiveMode::NonRecursive) {
                        log::warn!("failed to watch {}: {e}", path.display());
                        // If we fail to watch a directory path, don't keep trying to watch the parents.
                        break;
                    }

                    cur = parent;

                    // Load it now if it exists, because the notifier only loads it if
                    // it changed.
                    let path = parent.join(CONFIG_FILE_NAME);

                    // We watch the config path in addition to the parent directory. Some
                    // operations trigger notifications on the path itself, while others
                    // trigger notifications on the parent directory.
                    log::debug!("adding watcher for {}", path.display());
                    if let Err(e) = debouncer.watch(&path, RecursiveMode::NonRecursive) {
                        log::warn!("failed to watch {}: {e}", path.display());
                    }
                    Self::load_config(&self.configs, &path);
                }
            }
        }
    }

    pub fn config_for(&self, uri: &Url) -> Option<ContractConfig> {
        self.configs.read().unwrap().config_for(uri)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn from_str(s: &str) -> LocalConfig {
        let local_config: LocalConfigFormat = nickel_lang_core::deserialize::from_str(s).unwrap();
        local_config.into_local_config(Path::new("")).unwrap()
    }

    #[test]
    fn read_local_config() {
        let empty = from_str("[]");
        assert_eq!(empty, LocalConfig::default());

        let single_entry_with_setting =
            from_str(r#"[ { glob = "**/*.yaml", contract_path = "C.ncl" } ]"#);
        assert_eq!(
            &single_entry_with_setting
                .globs
                .into_iter()
                .next()
                .unwrap()
                .contract_path,
            Path::new("C.ncl")
        );
    }
}
