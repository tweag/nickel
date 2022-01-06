//! Import resolution. Search for imports in the AST, load the corresponding file in the cache, and
//! replace the original import node by a resolved import one, which stores the corresponding file
//! identifier directly.
use super::ImportResolver;
use crate::error::ImportError;
use crate::term::{RichTerm, Term, TraverseMethod};
use codespan::FileId;
use std::path::PathBuf;

/// The state passed around during the imports resolution. It holds a reference to the import
/// resolver, to a stack of pending imported term to be transformed and the path of the import
/// currently being processed, if any.
struct ImportsResolutionState<'a, R> {
    resolver: &'a mut R,
    stack: &'a mut Vec<FileId>,
    parent: Option<PathBuf>,
}

/// Import resolution.
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
    let mut stack = Vec::new();

    let source_file: Option<PathBuf> = rt.pos.as_opt_ref().map(|x| {
        let path = resolver.get_path(x.src_id);
        PathBuf::from(path)
    });
    let result = imports_pass(rt, resolver, &mut stack, source_file)?;

    Ok((result, stack))
}

/// Perform one full imports resolution pass. Put all imports encountered for the first time in
/// `stack`, but do not process them.
fn imports_pass<R>(
    rt: RichTerm,
    resolver: &mut R,
    stack: &mut Vec<FileId>,
    parent: Option<PathBuf>,
) -> Result<RichTerm, ImportError>
where
    R: ImportResolver,
{
    let mut state = ImportsResolutionState {
        resolver,
        stack,
        parent,
    };

    // If an import is resolved, then stack it.
    rt.traverse(
        &mut |rt: RichTerm,
              state: &mut ImportsResolutionState<R>|
         -> Result<RichTerm, ImportError> {
            let rt = transform_one(rt, state.resolver, &state.parent)?;

            if let Term::ResolvedImport(file_id) = rt.term.as_ref() {
                state.stack.push(*file_id);
            }
            Ok(rt)
        },
        &mut state,
        TraverseMethod::BottomUp,
    )
}

/// Resolve the import if the term is an unresolved import, or return the term unchanged. As
/// [`share_normal_form::transform_one`](../share_normal_form/fn.transform_one.html), this function
/// is not recursive.
pub fn transform_one<R>(
    rt: RichTerm,
    resolver: &mut R,
    parent: &Option<PathBuf>,
) -> Result<RichTerm, ImportError>
where
    R: ImportResolver,
{
    let term = rt.as_ref();
    match term {
        Term::Import(path) => {
            let (_, file_id) = resolver.resolve(path, parent.clone(), &rt.pos)?;
            Ok(RichTerm::new(Term::ResolvedImport(file_id), rt.pos))
        }
        _ => Ok(rt),
    }
}
