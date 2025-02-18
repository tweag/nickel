use std::collections::HashSet;

use nickel_lang_core::{
    cache::Caches,
    environment::Environment,
    identifier::Ident,
    position::TermPos,
    stdlib::{self, StdlibModule},
};

use crate::{analysis::AnalysisRegistry, cache::CachesExt as _, field_walker::Def};

pub(crate) fn initialize_stdlib(
    analysis: &mut AnalysisRegistry,
) -> Environment<Ident, Def> {
    cache.load_stdlib().unwrap();
    let initial_ctxt = cache.mk_type_ctxt().unwrap();
    let mut initial_env = Environment::default();

    for module in stdlib::modules() {
        let file_id = cache.get_submodule_file_id(module).unwrap();
        cache
            .typecheck_with_analysis(file_id, &initial_ctxt, &initial_env, analysis)
            .unwrap();

        // Add the std module to the environment (but not `internals`, because those symbols
        // don't get their own namespace, and we don't want to use them for completion anyway).
        if module == StdlibModule::Std {
            // The term should always be populated by typecheck_with_analysis.
            let term = cache.terms().get(&file_id).unwrap();
            let name = module.name().into();
            let def = Def::Let {
                ident: crate::identifier::LocIdent {
                    ident: name,
                    pos: TermPos::None,
                },
                metadata: Default::default(),
                value: term.term.clone(),
                path: Vec::new(),
            };
            initial_env.insert(name, def);
        }
    }
    initial_env
}

/// De-duplicate a vec without changing the order. The first instance of each unique
/// element will be kept.
pub fn dedup<T: std::hash::Hash + Eq + Clone>(xs: &mut Vec<T>) {
    let mut seen = HashSet::new();
    // Clone is needed because the signature of retain doesn't let us keep the reference.
    xs.retain(|x| seen.insert(x.clone()));
}
