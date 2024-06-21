//! Configuration for the Nickel Language Server
use serde::{Deserialize, Serialize};

use std::time::Duration;

/**
Limits to appy to the LSP background evaluator.
If an evaluation reaches one of these limits, it will be canceled and the offending file will be
temporarily blacklisted.
*/
#[derive(Debug, Deserialize, Serialize)]
#[serde(default)]
pub struct LspEvalLimits {
    /// Time out at which to cancel the background evaluation
    pub timeout: Duration,
    /// The maximum recursion level to allow in the background evaluator
    pub recursion_limit: usize,
}
impl Default for LspEvalLimits {
    fn default() -> Self {
        LspEvalLimits {
            timeout: Duration::from_secs(1),
            recursion_limit: 128,
        }
    }
}

/// The configuration of the LSP evaluator
#[derive(Debug, Deserialize, Serialize)]
#[serde(default)]
pub struct LspEvalConfig {
    pub eval_limits: LspEvalLimits,
    /// The duration during which a file that broke the background evaluator will be blacklisted
    /// from it
    pub blacklist_duration: Duration,
}

impl Default for LspEvalConfig {
    fn default() -> Self {
        LspEvalConfig {
            eval_limits: Default::default(),
            blacklist_duration: Duration::from_secs(30),
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Default)]
#[serde(default)]
pub struct LspConfig {
    /// Configuration for the background evaluator in the LSP
    pub eval_config: LspEvalConfig,
}
