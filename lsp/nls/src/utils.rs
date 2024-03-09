use nickel_lang_core::{
    cache_new::SourceCache,
    driver,
    environment::Environment,
    identifier::Ident,
    position::TermPos,
    source::SourcePath,
    stdlib::{self, StdlibModule},
    typecheck::Context,
};

use crate::{analysis::AnalysisRegistry, cache::CacheExt as _, field_walker::Def};

pub(crate) fn initialize_stdlib(
    cache: &mut SourceCache,
    analysis: &mut AnalysisRegistry,
) -> Environment<Ident, Def> {
    driver::load_stdlib(cache);
    let initial_ctxt = Context::from_stdlib(cache);
    let mut initial_env = Environment::default();

    for module in stdlib::modules() {
        let file_id = cache.find(&SourcePath::Std(module)).unwrap();
        cache
            .typecheck_with_analysis(file_id, &initial_ctxt, &initial_env, analysis)
            .unwrap();

        // Add the std module to the environment (but not `internals`, because those symbols
        // don't get their own namespace, and we don't want to use them for completion anyway).
        if module == StdlibModule::Std {
            // The term should always be populated by typecheck_with_analysis.
            let term = cache.term(file_id).unwrap();
            let name = module.name().into();
            let def = Def::Let {
                ident: crate::identifier::LocIdent {
                    ident: name,
                    pos: TermPos::None,
                },
                value: term.clone(),
                path: Vec::new(),
            };
            initial_env.insert(name, def);
        }
    }
    initial_env
}
