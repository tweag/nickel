//! Import resolution. Search for imports in the AST, load the corresponding file in the cache, and
//! replace the original import node by a resolved import one, which stores the corresponding file
//! identifier directly.
use super::ImportResolver;
use crate::error::ImportError;
use codespan::FileId;
use std::path::PathBuf;

/// The state passed around during the imports resolution. It holds a reference to the import
/// resolver, to a stack of pending imported term to be transformed and the path of the import
/// currently being processed, if any.
struct ImportsResolutionState<'a, R> {
    resolver: &'a mut R,
    stack: &'a mut Vec<FileId>,
    parent: Option<PathBuf>,
    unresolved: &'a mut Vec<ImportError>,
}

/// Performs import resolution, but return an error if any import terms cannot be resolved.
pub mod strict {
    use super::ImportResolver;
    use crate::error::ImportError;
    use crate::term::RichTerm;
    use codespan::FileId;
    use std::path::PathBuf;

    /// Perform imports resolution.
    ///
    /// All resolved imports are stacked during the process. Once the term has been traversed,
    /// the elements of this stack are returned. The caller is responsible
    /// to recursively resolve imports of this stack and or to perform
    /// transformations on it.
    pub fn resolve_imports<R>(
        rt: RichTerm,
        resolver: &mut R,
    ) -> Result<(RichTerm, Vec<FileId>), ImportError>
    where
        R: ImportResolver,
    {
        let (term, resolved, errors) = super::tolerant::resolve_imports(rt, resolver);
        match errors.first().cloned() {
            Some(first) => Err(first),
            None => Ok((term, resolved)),
        }
    }

    /// Resolve the import if the term is an unresolved import, or return the term unchanged. As
    /// [`super::share_normal_form::transform_one`], this function is not recursive.
    pub fn transform_one<R>(
        rt: RichTerm,
        resolver: &mut R,
        parent: &Option<PathBuf>,
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

/// Performs import resolution that cannot fail: it accumulates all errors and returns in
/// along side with a (partially) resolved term.
pub mod tolerant {
    use super::ImportResolver;
    use super::ImportsResolutionState;
    use crate::error::ImportError;
    use crate::term::{RichTerm, Term, Traverse, TraverseOrder};
    use codespan::FileId;
    use std::path::PathBuf;

    /// Performs imports resolution for each import term independent from others.
    /// It returns back a triple, containing:
    /// * A `RichTerm` which may still contain unresolved imports.
    /// * A `Vec<FileId>`, telling the imports which were resolved.
    /// * A `Vec<ImportError>`, telling the imports that couldn't be resolved.
    pub fn resolve_imports<R>(
        rt: RichTerm,
        resolver: &mut R,
    ) -> (RichTerm, Vec<FileId>, Vec<ImportError>)
    where
        R: ImportResolver,
    {
        let mut stack = Vec::new();
        let mut unresolved = Vec::new();

        let source_file: Option<PathBuf> = rt.pos.as_opt_ref().map(|x| {
            let path = resolver.get_path(x.src_id);
            PathBuf::from(path)
        });

        let mut state = ImportsResolutionState {
            resolver,
            stack: &mut stack,
            parent: source_file,
            unresolved: &mut unresolved,
        };

        // If an import is resolved, then stack it.
        let transformed = rt
            .traverse(
                &|rt: RichTerm,
                  state: &mut ImportsResolutionState<R>|
                 -> Result<RichTerm, ImportError> {
                    let (rt, err) = transform_one(rt, state.resolver, &state.parent);
                    if let Some(err) = err {
                        state.unresolved.push(err);
                    }

                    if let Term::ResolvedImport(file_id) = rt.term.as_ref() {
                        state.stack.push(*file_id);
                    }
                    Ok(rt)
                },
                &mut state,
                TraverseOrder::BottomUp,
            )
            // This will always succeed, becuase we always return `Ok(..)`, and
            // we don't use the `?` operator
            .unwrap();

        (transformed, stack, unresolved)
    }

    /// Try to resolve an import if the term is an unresolved import, and return
    /// back the a potentially resolved richterm and an import error if it couldn't
    /// be unresolved import could not be resolved.
    pub fn transform_one<R>(
        rt: RichTerm,
        resolver: &mut R,
        parent: &Option<PathBuf>,
    ) -> (RichTerm, Option<ImportError>)
    where
        R: ImportResolver,
    {
        let term = rt.as_ref();
        match term {
            Term::Import(path) => match resolver.resolve(path, parent.clone(), &rt.pos) {
                Ok((_, file_id)) => (RichTerm::new(Term::ResolvedImport(file_id), rt.pos), None),
                Err(err) => (rt, Some(err)),
            },
            _ => (rt, None),
        }
    }
}
