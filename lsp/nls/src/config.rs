//! Configuration for the Nickel Language Server
use lsp_types::Url;
use nickel_lang_core::{combine::Combine, term::IndexMap};
use notify_debouncer_full::{
    notify::{EventKind, RecommendedWatcher, RecursiveMode},
    DebouncedEvent, Debouncer, RecommendedCache,
};
use serde::{de::Error, Deserialize, Deserializer, Serialize};

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::{Arc, RwLock},
    time::Duration,
};

use crate::files::uri_to_path;

fn deserialize_friendly_duration<'de, D: Deserializer<'de>>(
    de: D,
) -> Result<Option<Duration>, D::Error> {
    let signed = jiff::SignedDuration::deserialize(de)?;
    Some(signed.try_into().map_err(D::Error::custom)).transpose()
}

/// Limits to apply to the LSP background evaluator. If an evaluation reaches one of these limits,
/// it will be canceled and the offending file will be temporarily blacklisted.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Default)]
#[serde(default)]
pub struct LspEvalLimits {
    #[serde(deserialize_with = "deserialize_friendly_duration")]
    timeout: Option<Duration>,
    recursion_limit: Option<usize>,
}

impl LspEvalLimits {
    /// How long should we allow background evaluation to run before cancelling it?
    pub fn timeout(&self) -> Duration {
        self.timeout.unwrap_or(Duration::from_secs(1))
    }

    /// The maximum recursion level to allow in the background evaluator
    pub fn recursion_limit(&self) -> usize {
        self.recursion_limit.unwrap_or(128)
    }
}

impl Combine for LspEvalLimits {
    fn combine(left: Self, right: Self) -> Self {
        Self {
            timeout: left.timeout.or(right.timeout),
            recursion_limit: left.recursion_limit.or(right.recursion_limit),
        }
    }
}

/// The configuration of the LSP evaluator
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Default)]
#[serde(default)]
pub struct LspEvalConfig {
    disable: Option<bool>,
    #[serde(deserialize_with = "deserialize_friendly_duration")]
    blacklist_duration: Option<Duration>,
    pub eval_limits: LspEvalLimits,
}

impl LspEvalConfig {
    /// Whether to disable background evaluation.
    pub fn disable(&self) -> bool {
        self.disable.unwrap_or_default()
    }

    /// When a file fails evaluation (e.g. by timing out or crashing), how long
    /// should we wait until trying again?
    pub fn blacklist_duration(&self) -> Duration {
        self.blacklist_duration.unwrap_or(Duration::from_secs(30))
    }
}

impl Combine for LspEvalConfig {
    fn combine(left: Self, right: Self) -> Self {
        Self {
            disable: left.disable.or(right.disable),
            eval_limits: Combine::combine(left.eval_limits, right.eval_limits),
            blacklist_duration: left.blacklist_duration.or(right.blacklist_duration),
        }
    }
}

/// Global configuration for nls.
///
/// This is provided by the editor on startup.
#[derive(Clone, Debug, Deserialize, Serialize, Default, PartialEq)]
#[serde(default)]
pub struct LspConfig {
    /// Configuration for the background evaluator in the LSP
    pub eval_config: LspEvalConfig,
}

/// File-local configuration for nls.
///
/// This takes priority over the global configuration, and is obtained
/// from configuration files on the filesystem. See [`LocalConfigs`].
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
#[serde(default)]
pub struct FileConfig {
    /// Configuration for the background evaluator in the LSP
    pub eval_config: LspEvalConfig,
}

impl FileConfig {
    fn apply_global_config(&mut self, global: LspConfig) {
        self.eval_config = Combine::combine(self.eval_config.clone(), global.eval_config);
    }
}

/// A collection of file-local nls configurations, provided by a single
/// config file.
///
/// When we find a config file (called `nls.ncl`) in the filesystem,
/// we look inside for a "glob -> config" mapping, and apply the
/// provided config to every file matching the glob.
///
/// For example, if you have a directory containing `nls.ncl` and
/// `foo.ncl`, and `nls.ncl` has contents
///
/// ```ncl
/// {
///   "*o.ncl" = { eval_config.eval_limits.timeout = "10 seconds" }
/// }
/// ```
///
/// then the background eval job for `foo.ncl` will have an evalution
/// timeout of 10 seconds.
#[derive(Debug, Default, PartialEq)]
pub struct LocalConfig {
    globs: IndexMap<glob::Pattern, FileConfig>,
}

/// The serialization format for [`LocalConfig`].
#[derive(Debug, Deserialize)]
#[serde(transparent)]
struct LocalConfigFormat {
    globs: IndexMap<String, FileConfig>,
}

impl TryFrom<LocalConfigFormat> for LocalConfig {
    type Error = anyhow::Error;

    fn try_from(conf: LocalConfigFormat) -> Result<Self, Self::Error> {
        Ok(LocalConfig {
            globs: conf
                .globs
                .into_iter()
                .map(|(glob, config)| Ok((glob.parse()?, config)))
                .collect::<Result<_, glob::PatternError>>()?,
        })
    }
}

impl LocalConfig {
    fn from_file(path: &Path) -> Result<Self, anyhow::Error> {
        let local_config: LocalConfigFormat = nickel_lang_core::deserialize::from_path(path)
            // Eagerly format the error, because anyhow requires `Send` errors and ours aren't
            .map_err(|e| anyhow::anyhow!("{}", e.to_string()))?;
        local_config.try_into()
    }
}

pub struct LocalConfigs {
    configs: HashMap<PathBuf, LocalConfig>,
    global_config: LspConfig,
}

/// A self-updating collection of [`LocalConfig`]s.
pub struct LocalConfigsWatcher {
    configs: Arc<RwLock<LocalConfigs>>,
    // The collection of directories where we're already watching
    // for a config file.
    already_watched: HashSet<PathBuf>,
    debouncer: Option<Debouncer<RecommendedWatcher, RecommendedCache>>,
}

impl LocalConfigs {
    fn default_file_config(&self) -> FileConfig {
        FileConfig {
            eval_config: self.global_config.eval_config.clone(),
        }
    }

    pub fn config_for(&self, uri: &Url) -> FileConfig {
        let Ok(path) = uri_to_path(uri) else {
            return self.default_file_config();
        };
        let mut cur = path.as_path();
        while let Some(parent) = cur.parent() {
            if let Some(nls_conf) = self.configs.get(parent) {
                for (glob, config) in &nls_conf.globs {
                    if let Ok(relative) = path.strip_prefix(parent) {
                        if glob.matches_path(relative) {
                            return config.clone();
                        }
                    }
                }
            }
            cur = parent;
        }
        self.default_file_config()
    }

    pub fn insert(&mut self, path: PathBuf, mut config: LocalConfig) {
        for file_config in config.globs.values_mut() {
            file_config.apply_global_config(self.global_config.clone());
        }
        self.configs.insert(path, config);
    }

    pub fn remove(&mut self, path: &Path) {
        self.configs.remove(path);
    }
}

impl LocalConfigsWatcher {
    pub fn new(global_config: LspConfig) -> Self {
        let configs = Arc::new(RwLock::new(LocalConfigs {
            configs: HashMap::new(),
            global_config,
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
                            .find(|p| p.file_name() == Some("nls.ncl".as_ref()))
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
                    }
                    cur = parent;

                    // Load it now if it exists, because the notifier only loads it if
                    // it changed.
                    let path = parent.join("nls.ncl");
                    Self::load_config(&self.configs, &path);
                }
            }
        }
    }

    pub fn config_for(&self, uri: &Url) -> FileConfig {
        self.configs.read().unwrap().config_for(uri)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn from_str(s: &str) -> LocalConfig {
        let local_config: LocalConfigFormat = nickel_lang_core::deserialize::from_str(s).unwrap();
        local_config.try_into().unwrap()
    }

    #[test]
    fn read_local_config() {
        let empty = from_str("{}");
        assert_eq!(empty, LocalConfig::default());

        let single_empty_entry = from_str(r#"{ "**/*.yaml" = {} }"#);
        assert_eq!(
            single_empty_entry.globs.into_values().next().unwrap(),
            FileConfig::default()
        );

        let single_entry_with_setting =
            from_str(r#"{ "**/*.yaml" = { eval_config.eval_limits.timeout = "1 second" } }"#);
        assert_eq!(
            single_entry_with_setting
                .globs
                .into_values()
                .next()
                .unwrap()
                .eval_config
                .eval_limits
                .timeout(),
            Duration::from_secs(1)
        );
    }

    #[test]
    fn deserialize_eval_limits() {
        let limits: LspEvalLimits = serde_json::from_str(r#"{ "recursion_limit": 1 }"#).unwrap();
        assert_eq!(limits.recursion_limit(), 1);
        assert!(limits.timeout.is_none());
    }

    #[test]
    fn deserialize_eval_config() {
        let config: LspEvalConfig =
            serde_json::from_str(r#"{ "eval_limits": { "recursion_limit": 1 } }"#).unwrap();
        assert_eq!(config.eval_limits.recursion_limit(), 1);
        assert!(config.eval_limits.timeout.is_none());
    }
}
