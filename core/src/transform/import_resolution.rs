//! Import resolution. Search for imports in the AST, load the corresponding file in the cache, and
//! replace the original import node by a resolved import one, which stores the corresponding file
//! identifier directly.
use super::ImportResolver;

/// Performs import resolution, but return an error if any import terms cannot be resolved.
pub mod strict {
    use super::{tolerant, ImportResolver};
    use crate::error::ImportError;
    use crate::term::RichTerm;
    use codespan::FileId;

    /// The result of an import resolution transformation.
    #[derive(Debug)]
    pub struct ResolveResult {
        /// The transformed term after the import resolution transformation.
        pub transformed_term: RichTerm,
        /// Imports that were resolved without errors, but are potentially not yet transformed.
        pub resolved_ids: Vec<FileId>,
    }

    impl From<tolerant::ResolveResult> for ResolveResult {
        fn from(result_tolerant: tolerant::ResolveResult) -> Self {
            ResolveResult {
                transformed_term: result_tolerant.transformed_term,
                resolved_ids: result_tolerant.resolved_ids,
            }
        }
    }

    impl From<ResolveResult> for tolerant::ResolveResult {
        fn from(result_strict: ResolveResult) -> Self {
            tolerant::ResolveResult {
                transformed_term: result_strict.transformed_term,
                resolved_ids: result_strict.resolved_ids,
                import_errors: Vec::new(),
            }
        }
    }

    /// Perform imports resolution.
    ///
    /// All resolved imports are stacked during the process. Once the term has been traversed, the
    /// elements of this stack are returned. The caller is responsible for performing
    /// transformations (including import resolution) on the resolved terms, if needed.
    pub fn resolve_imports<R>(rt: RichTerm, resolver: &mut R) -> Result<ResolveResult, ImportError>
    where
        R: ImportResolver,
    {
        let tolerant_result = super::tolerant::resolve_imports(rt, resolver);
        match tolerant_result.import_errors.first() {
            Some(err) => Err(err.clone()),
            None => Ok(tolerant_result.into()),
        }
    }

    /// Resolve the import if the term is an unresolved import, or return the term unchanged. This
    /// function is not recursive, and is to be used in conjunction with e.g.
    /// [crate::term::Traverse].
    pub fn transform_one<R>(
        rt: RichTerm,
        resolver: &mut R,
        parent: Option<FileId>,
    ) -> Result<RichTerm, ImportError>
    where
        R: ImportResolver,
    {
        let (rt, error) = super::tolerant::transform_one(rt, resolver, parent);
        match error {
            Some(err) => Err(err),
            None => Ok(rt),
        }
    }
}

/// Performs import resolution that cannot fail: it accumulates all errors and returns them
/// together with a (partially) resolved term.
pub mod tolerant {
    use super::ImportResolver;
    use crate::error::ImportError;
    use crate::term::{RichTerm, Term, Traverse, TraverseOrder};
    use codespan::FileId;

    /// The result of an error tolerant import resolution.
    #[derive(Debug)]
    pub struct ResolveResult {
        /// The main result of the transformation. May still contain unresolved import terms in
        /// case of errors.
        pub transformed_term: RichTerm,
        /// Imports that were resolved without errors, but are potentially yet to be transformed.
        pub resolved_ids: Vec<FileId>,
        /// Errors produced when failing to resolve imports.
        pub import_errors: Vec<ImportError>,
    }

    /// Performs imports resolution in an error tolerant way.
    pub fn resolve_imports<R>(rt: RichTerm, resolver: &mut R) -> ResolveResult
    where
        R: ImportResolver,
    {
        let mut stack = Vec::new();
        let mut import_errors = Vec::new();

        let source_file = rt.pos.as_opt_ref().map(|x| x.src_id);

        // If an import is resolved, then stack it.
        let transformed = rt
            .traverse(
                &mut |rt: RichTerm| -> Result<RichTerm, ImportError> {
                    let (rt, err) = transform_one(rt, resolver, source_file);
                    if let Some(err) = err {
                        import_errors.push(err);
                    }

                    if let Term::ResolvedImport(file_id) = rt.term.as_ref() {
                        stack.push(*file_id);
                    }
                    Ok(rt)
                },
                TraverseOrder::BottomUp,
            )
            // This will always succeed, because we always return `Ok(..)`, and
            // we don't use the `?` operator
            .unwrap();

        ResolveResult {
            transformed_term: transformed,
            resolved_ids: stack,
            import_errors,
        }
    }

    /// Try to resolve an import if the term is an unresolved import. Returns a resolved import
    /// term if the term was an unresolved import. If the term wasn't an unresolved import, or if
    /// the resolution fails, the original term is returned unchanged. In the latter case, the
    /// error is returned as well, as a second component of the tuple.
    pub fn transform_one<R>(
        rt: RichTerm,
        resolver: &mut R,
        parent: Option<FileId>,
    ) -> (RichTerm, Option<ImportError>)
    where
        R: ImportResolver,
    {
        let term = rt.as_ref();
        match term {
            Term::Import(path) => match resolver.resolve(path, parent, &rt.pos) {
                Ok((_, file_id)) => (RichTerm::new(Term::ResolvedImport(file_id), rt.pos), None),
                Err(err) => (rt, Some(err)),
            },
            _ => (rt, None),
        }
    }
}
