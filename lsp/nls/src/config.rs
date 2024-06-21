//! Configuration for the nls server
use serde::{Deserialize, Serialize};

use std::time::Duration;

fn default_eval_timeout() -> Duration {
    Duration::from_secs(1)
}
fn default_recursion_limit() -> usize {
    128
}
fn default_blacklist_duration() -> Duration {
    Duration::from_secs(30)
}

/**
Limits to appy to the LSP background evaluator.
If an evaluation reaches one of these limits, it will be canceled and the offending file will be
temporarily blacklisted.
*/
#[derive(Debug, Deserialize, Serialize)]
pub struct LspEvalLimits {
    /// Time out at which to cancel the background evaluation
    #[serde(default = "default_eval_timeout")]
    pub timeout: Duration,
    /// The maximum recursion level to allow in the background evaluator
    #[serde(default = "default_recursion_limit")]
    pub recursion_limit: usize,
}
impl Default for LspEvalLimits {
    fn default() -> Self {
        LspEvalLimits {
            timeout: default_eval_timeout(),
            recursion_limit: default_recursion_limit(),
        }
    }
}

/// The configuration of the LSP evaluator
#[derive(Debug, Deserialize, Serialize)]
pub struct LspEvalConfig {
    #[serde(default)]
    pub eval_limits: LspEvalLimits,
    /// The duration during which a file that broke the background evaluator will be blacklisted
    /// from it
    #[serde(default = "default_blacklist_duration")]
    pub blacklist_duration: Duration,
}

impl Default for LspEvalConfig {
    fn default() -> Self {
        LspEvalConfig {
            eval_limits: Default::default(),
            // The duration during which a file causing the evaluator to timeout will be blacklisted from further
            // evaluations
            blacklist_duration: default_blacklist_duration(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LspConfig {
    #[serde(default)]
    pub eval_config: LspEvalConfig,
}

impl Default for LspConfig {
    fn default() -> Self {
        LspConfig {
            eval_config: Default::default(),
        }
    }
}
