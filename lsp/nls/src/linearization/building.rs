use std::collections::HashMap;

use nickel::typecheck::linearization::{LinearizationState, ScopeId};

use super::{interface::Unresolved, LinearizationItem};

/// A concrete [LinearizationState]
/// Holds any inner datatype that can be used as stable resource
/// while recording terms.
#[derive(Default)]
pub struct Building {
    pub linearization: Vec<LinearizationItem<Unresolved>>,
    pub scope: HashMap<Vec<ScopeId>, Vec<usize>>,
}

impl LinearizationState for Building {}
