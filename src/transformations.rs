use crate::cache::ImportResolver;
use crate::error::ImportError;
use crate::eval::{Closure, Environment, IdentKind, Thunk};
use crate::identifier::Ident;
use crate::term::{Contract, RichTerm, Term, TraverseMethod};
use crate::types::{AbsType, Types};
use codespan::FileId;
use simple_counter::*;
use std::path::PathBuf;

generate_counter!(FreshVarCounter, usize);

/// # Desugarise destructuring
///
/// Replace a let-binding with destructuring by a classical let-binding.
/// It will first destruct the pattern and create a new var for each field of the patternn.
/// After that, it will construct a new Record/List from the extracted fields.
///
/// ## Example:
///
/// Taking the let pattern:
/// ```text
/// let x @ {a, b=d, ..} = {a=1,b=2,c="ignored"} in ...
/// ```
/// after transformation we will have:
/// ```text
/// let x = {a=1,b=2,c="ignored"} in
/// let a = x.a in
/// let d = x.b in
/// ...
/// ```
pub mod desugar_destructuring {
    use super::{Ident, RichTerm, Term};
    use crate::destruct::{Destruct, Match};
    use crate::term::make::{op1, op2};
    use crate::term::MetaValue;
    use crate::term::{BinaryOp::DynRemove, UnaryOp::StaticAccess};

    /// Wrap the desugarized term in a meta value containing the "Record contract" needed to check
    /// the pattern exaustivity and also fill the default values (`?` operator) if not presents in the record.
    /// This function should be, in the general case, considered as the entry point of this
    /// transformation.
    pub fn desugar_with_contract(rt: RichTerm) -> RichTerm {
        if let Term::LetPattern(x, pat, t_, body) = *rt.term {
            let pos = body.pos;
            let meta = pat.clone().as_contract();
            let t_ = {
                let t_pos = t_.pos;
                RichTerm::new(
                    Term::MetaValue(MetaValue {
                        value: Some(t_),
                        ..meta
                    }),
                    t_pos,
                )
            };
            desugar(RichTerm::new(Term::LetPattern(x, pat, t_, body), pos))
        } else {
            rt
        }
    }

    /// Main transformation function to desuggar let patterns.
    /// WARNING: In a real usage case, you will want to generate also the matavalue associated to
    /// this pattern destructuring. Do not concider this function as the entry point of the
    /// transformation. For that, use `desugar_with_contract`.
    pub fn desugar(rt: RichTerm) -> RichTerm {
        if let Term::LetPattern(x, pat, t_, body) = *rt.term {
            let pos = body.pos;
            let x = x.unwrap_or_else(super::fresh_var);
            RichTerm::new(
                Term::Let(
                    x.clone(),
                    t_,
                    destruct_term(x.clone(), &pat, bind_open_field(x, &pat, body)),
                ),
                pos,
            )
        } else {
            rt
        }
    }

    /// Wrap `body` in a let construct binding the open part of the pattern to the required value.
    /// Having `let {a,..y} = {a=1, b=2, c=3} in <BODY>` will bind `y` to {b=2,c=3} in `BODY`.
    /// Here, the x, is the identifier pointing to the full record. If having `val @ {...} = ... in
    /// ...` the param x should be `Ident("val")` but if we have a `@` binding less form, you will
    /// probably generate a fresh variable.
    fn bind_open_field(x: Ident, pat: &Destruct, body: RichTerm) -> RichTerm {
        let (matches, var) = match pat {
            Destruct::Record(matches, true, Some(x)) => (matches, x.clone()),
            Destruct::Record(matches, true, None) => (matches, super::fresh_var()),
            Destruct::Record(_, false, None) | Destruct::Empty => return body,
            _ => panic!("A closed pattern can not have a rest binding"),
        };
        Term::Let(
            var.clone(),
            matches.iter().fold(Term::Var(x).into(), |x, m| match m {
                Match::Simple(i, _) | Match::Assign(i, _, _) => {
                    op2(DynRemove(), Term::Str(i.to_string()), x)
                }
            }),
            body,
        )
        .into()
    }

    /// Core of the destructuring. Bind all the variables of the pattern except the "open" (`..y`)
    /// part. For that, see `bind_open_field`.
    fn destruct_term(x: Ident, pat: &Destruct, body: RichTerm) -> RichTerm {
        let pos = body.pos;
        match pat {
            Destruct::Record(matches, ..) => matches.iter().fold(body, move |t, m| match m {
                Match::Simple(id, _) => RichTerm::new(
                    Term::Let(
                        id.clone(),
                        op1(StaticAccess(id.clone()), Term::Var(x.clone())),
                        t,
                    ),
                    pos,
                ),
                Match::Assign(f, _, (id, pat)) => desugar(RichTerm::new(
                    Term::LetPattern(
                        id.clone(),
                        pat.clone(),
                        op1(StaticAccess(f.clone()), Term::Var(x.clone())),
                        t,
                    ),
                    pos,
                )),
            }),
            _ => body,
        }
    }
}

/// Share normal form.
///
/// Replace the subexpressions of WHNFs that are not functions by thunks, such that they can be
/// shared. It is similar to the behavior of other lazy languages with respect to data
/// constructors.  To do so, subexpressions are replaced by fresh variables, introduced by new let
/// bindings put at the beginning of the WHNF.
///
/// For example, take the expression
/// ```text
/// let x = {a = (1 + 1);} in x.a + x.a
/// ```
///
/// The term `{a = 1 + 1;}` is a record, and hence a WHNF. In consequence, the thunk allocated to x
/// is never updated. Without additional machinery, `a` will be recomputed each time is it used,
/// two times here.
///
/// The transformation replaces such subexpressions, namely the content of the fields
/// of records and the elements of lists - `(1 + 1)` in our example -, with fresh variables
/// introduced by `let`  added at the head of the term:
///
/// ```text
/// let x = (let var = 1 + 1 in {a = var;}) in x.a + x.a
/// ```
///
/// Now, the field `a` points to the thunk introduced by `var`: at the evaluation of the first
/// occurrence of `x.a`, this thunk is updated with `2`, and is not recomputed the second time.
///
/// Newly introduced variables begin with a special character to avoid clashing with user-defined
/// variables.
pub mod share_normal_form {
    use super::fresh_var;
    use crate::identifier::Ident;
    use crate::position::TermPos;
    use crate::term::{MetaValue, RichTerm, Term};

    /// Transform the top-level term of an AST to a share normal form, if it can.
    ///
    /// This function is not recursive: it just tries to apply one step of the transformation to
    /// the top-level node of the AST. For example, it transforms `[1 + 1, [1 + 2]]` to `let %0 = 1
    /// + 1 in [%0, [1 + 2]]`: the nested subterm `[1 + 2]` is left as it was. If the term is
    /// neither a record, a list nor an enriched value, it is returned the same.  In other words,
    /// the transformation is implemented as rewrite rules, and must be used in conjunction a
    /// traversal to obtain a full transformation.
    pub fn transform_one(rt: RichTerm) -> RichTerm {
        let RichTerm { term, pos } = rt;
        match *term {
            Term::Record(map, attrs) => {
                let mut bindings = Vec::with_capacity(map.len());

                let map = map
                    .into_iter()
                    .map(|(id, t)| {
                        if should_share(&t.term) {
                            let fresh_var = fresh_var();
                            let pos_t = t.pos;
                            bindings.push((fresh_var.clone(), t));
                            (id, RichTerm::new(Term::Var(fresh_var), pos_t))
                        } else {
                            (id, t)
                        }
                    })
                    .collect();

                with_bindings(Term::Record(map, attrs), bindings, pos, BindingType::Normal)
            }
            Term::RecRecord(map, dyn_fields, attrs) => {
                // When a recursive record is evaluated, all fields need to be turned to closures
                // anyway (see the corresponding case in `eval::eval()`), which is what the share
                // normal form transformation does. This is why the test is more lax here than for
                // other constructors: it is not only about sharing, but also about the future
                // evaluation of recursive records. Only constant are not required to be
                // closurized.
                let mut bindings = Vec::with_capacity(map.len());

                let map = map
                    .into_iter()
                    .map(|(id, t)| {
                        if !t.as_ref().is_constant() {
                            let fresh_var = fresh_var();
                            let pos_t = t.pos;
                            bindings.push((fresh_var.clone(), t));
                            (id, RichTerm::new(Term::Var(fresh_var), pos_t))
                        } else {
                            (id, t)
                        }
                    })
                    .collect();

                let dyn_fields = dyn_fields
                    .into_iter()
                    .map(|(id_t, t)| {
                        if !t.as_ref().is_constant() {
                            let fresh_var = fresh_var();
                            let pos_t = t.pos;
                            bindings.push((fresh_var.clone(), t));
                            (id_t, RichTerm::new(Term::Var(fresh_var), pos_t))
                        } else {
                            (id_t, t)
                        }
                    })
                    .collect();

                // Recursive records are the reason why we need revertible thunks, since when
                // merged, we may have to revert the fields back to their original expression.
                with_bindings(
                    Term::RecRecord(map, dyn_fields, attrs),
                    bindings,
                    pos,
                    BindingType::Revertible,
                )
            }
            Term::List(ts) => {
                let mut bindings = Vec::with_capacity(ts.len());

                let ts = ts
                    .into_iter()
                    .map(|t| {
                        if should_share(&t.term) {
                            let fresh_var = fresh_var();
                            let pos_t = t.pos;
                            bindings.push((fresh_var.clone(), t));
                            RichTerm::new(Term::Var(fresh_var), pos_t)
                        } else {
                            t
                        }
                    })
                    .collect();

                with_bindings(Term::List(ts), bindings, pos, BindingType::Normal)
            }
            Term::MetaValue(mut meta @ MetaValue { value: Some(_), .. }) => {
                if meta.value.as_ref().map(|t| should_share(&t.term)).unwrap() {
                    let fresh_var = fresh_var();
                    let t = meta.value.take().unwrap();
                    meta.value
                        .replace(RichTerm::new(Term::Var(fresh_var.clone()), t.pos));
                    let inner = RichTerm::new(Term::MetaValue(meta), pos);
                    RichTerm::new(Term::Let(fresh_var, t, inner), pos)
                } else {
                    RichTerm::new(Term::MetaValue(meta), pos)
                }
            }
            t => RichTerm::new(t, pos),
        }
    }

    /// Determine if a subterm of a WHNF should be wrapped in a thunk in order to be shared.
    ///
    /// Sharing is typically useless if the subterm is already a WHNF which can be copied without
    /// duplicating any work. On the other hand, a WHNF which can contain other shareable
    /// subexpressions, such as a record, should be shared.
    fn should_share(t: &Term) -> bool {
        match t {
            Term::Null
            | Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Lbl(_)
            | Term::Sym(_)
            | Term::Var(_)
            | Term::Enum(_)
            | Term::Fun(_, _) => false,
            _ => true,
        }
    }

    /// Type of let-binding to introduce during the share normal form pass.
    enum BindingType {
        Normal,
        Revertible,
    }

    impl Default for BindingType {
        fn default() -> Self {
            BindingType::Normal
        }
    }

    /// Bind a list of pairs `(identifier, term)` in a term.
    ///
    /// Given the term `body` and bindings of identifiers to terms represented as a list of pairs
    /// `(id_1, term_1), .., (id_n, term_n)`, return the new term `let id_n = term_n in ... let
    /// id_1 = term_1 in body`.
    fn with_bindings(
        body: Term,
        bindings: Vec<(Ident, RichTerm)>,
        pos: TermPos,
        btype: BindingType,
    ) -> RichTerm {
        bindings.into_iter().fold(
            RichTerm {
                term: Box::new(body),
                pos: pos.into_inherited(),
            },
            |acc, (id, t)| match btype {
                BindingType::Normal => RichTerm::new(Term::Let(id, t, acc), pos),
                BindingType::Revertible => RichTerm::new(Term::LetRev(id, t, acc), pos),
            },
        )
    }
}

pub mod import_resolution {
    use super::{ImportResolver, PathBuf, RichTerm, Term};
    use crate::error::ImportError;

    /// Resolve the import if the term is an unresolved import, or return the term unchanged.
    /// As [`share_normal_form::transform_one`](../share_normal_form/fn.transform_one.html),
    /// this function is not recursive.
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
}

/// During the evaluation, we the following invariant is enforced: any contract (be it the type
/// annotation, or the contracts) contained in a `MetaValue` must have been applied to the inner
/// value of this metavalue. This invariant is false just after parsing, as there's merely no
/// direct `Assume` in the output AST. This transformation makes it true after program
/// transformations by generating corresponding assume.
///
/// It must be run before `share_normal_form` to avoid rechecking contracts each time the inner
/// value is unwrapped.
pub mod apply_contracts {
    use super::{RichTerm, Term};
    use crate::mk_app;

    /// If the top-level node of the AST is a meta-value, apply the meta-value's contracts to the
    /// inner value.  Otherwise, return the term unchanged.
    pub fn transform_one(rt: RichTerm) -> RichTerm {
        let RichTerm { term, pos } = rt;

        match *term {
            Term::MetaValue(mut meta) if meta.value.is_some() => {
                let inner = meta.types.iter().chain(meta.contracts.iter()).fold(
                    meta.value.take().unwrap(),
                    |acc, ctr| {
                        mk_app!(
                            ctr.types.clone().contract(),
                            Term::Lbl(ctr.label.clone()),
                            acc
                        )
                        .with_pos(pos.into_inherited())
                    },
                );

                meta.value.replace(inner);
                RichTerm::new(Term::MetaValue(meta), pos.into_inherited())
            }
            t => RichTerm::new(t, pos),
        }
    }
}

/// The state passed around during the imports resolution. It holds a reference to the import
/// resolver, to a stack of pending imported term to be transformed and the path of the import
/// currently being processed, if any.
struct ImportsResolutionState<'a, R> {
    resolver: &'a mut R,
    stack: &'a mut Vec<FileId>,
    parent: Option<PathBuf>,
}

/// Apply all program transformations, which are currently the share normal form transformations and
/// contracts application.
/// Do not perform transformation on the imported files.
/// If needed, either do it yourself using pending imports returned by
/// [`resolve_imports`](../fn.resolve_imports.html)
/// or use the [`Cache`](../../cache/struct.Cache.html)
pub fn transform(rt: RichTerm) -> RichTerm {
    let rt = rt
        .traverse(
            &mut |rt: RichTerm, _| -> Result<RichTerm, ()> {
                // before anything, we have to desugar the syntax
                let rt = desugar_destructuring::desugar_with_contract(rt);
                // We need to do contract generation before wrapping stuff in variables
                let rt = apply_contracts::transform_one(rt);
                Ok(rt)
            },
            &mut (),
            TraverseMethod::TopDown,
        )
        .unwrap();
    rt.traverse(
        &mut |rt: RichTerm, _| -> Result<RichTerm, ()> {
            let rt = share_normal_form::transform_one(rt);
            Ok(rt)
        },
        &mut (),
        TraverseMethod::BottomUp,
    )
    .unwrap()
}

/// import resolution.
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
            let rt = import_resolution::transform_one(rt, state.resolver, &state.parent)?;

            if let Term::ResolvedImport(file_id) = rt.term.as_ref() {
                state.stack.push(*file_id);
            }
            Ok(rt)
        },
        &mut state,
        TraverseMethod::BottomUp,
    )
}

/// Generate a new fresh variable which do not clash with user-defined variables.
pub fn fresh_var() -> Ident {
    use crate::identifier::GEN_PREFIX;

    format!("{}{}", GEN_PREFIX, FreshVarCounter::next()).into()
}

/// Structures which can be packed together with their environment as a closure.
///
/// The typical implementer is [`RichTerm`](../term/enum.RichTerm.html), but structures containing
/// terms can also be closurizable, such as the contract in a [`Types`](../types/typ.Types.html).
/// In this case, the inner term is closurized.
pub trait Closurizable {
    /// Pack a closurizable together with its environment `with_env` as a closure in the main
    /// environment `env`.
    fn closurize(self, env: &mut Environment, with_env: Environment) -> Self;
}

impl Closurizable for RichTerm {
    /// Pack a term together with an environment as a closure.
    ///
    /// Generate a fresh variable, bind it to the corresponding closure `(t,with_env)` in `env`,
    /// and return this variable as a fresh term.
    fn closurize(self, env: &mut Environment, with_env: Environment) -> RichTerm {
        // If the term is already a variable, we don't have to create a useless intermediate
        // closure. We just transfer the original thunk to the new environment. This is not only an
        // optimization: this is relied upon by recursive record merging when computing the
        // fixpoint.
        //
        // More specifically, the evaluation of a recursive record patches the environment of each
        // field with the thunks recursively referring to the other fields of the record. `eval`
        // assumes that a recursive record field is either a constant or a generated variable whose
        // thunks *immediately* contain the original unevaluated expression (both properties are
        // true after the share normal form transformation and maintained when reverting thunks
        // before merging recursive records).
        //
        // To maintain this invariant, `closurize` must NOT introduce an indirection through a
        // variable, such as transforming:
        //
        // ```
        // {foo = %1, bar = 1} in env %1 <- 1 + bar
        // ```
        //
        // to:
        //
        // ```
        // {foo = %2, bar = 1} in env %2 <- (%1 in env %1 <- 1 + bar)
        // ```
        //
        // In this case, the evaluation of the recursive records will patch the outer environment
        // instead of the inner one, giving:
        //
        // ```
        // {foo = %2, bar = 1} in env %2 <- (%1 in env %1 <- 1 + bar), bar <- 1
        // ```
        //
        // Then, evaluating `foo` would unduly raise an unbound identifier error.
        //
        //
        // We currently only do this optimization for generated variables (introduced by the share
        // normal form). We could do it for non-generated identifiers as well, but be we would be
        // renaming user-supplied variables by gibberish generated names. It may hamper error
        // reporting, so for the time being, we restrict ourselves to generated identifiers.
        let var = fresh_var();
        let pos = self.pos;

        let thunk = match self.as_ref() {
            Term::Var(id) if id.is_generated() => with_env.get(&id).expect(&format!(
                "Internal error(closurize) : generated identifier {} not found in the environment",
                id.is_generated()
            )),
            _ => {
                let closure = Closure {
                    body: self,
                    env: with_env,
                };
                Thunk::new(closure, IdentKind::Record())
            }
        };

        env.insert(var.clone(), thunk);
        RichTerm::new(Term::Var(var), pos.into_inherited())
    }
}

impl Closurizable for Types {
    /// Pack the contract of a type together with an environment as a closure.
    ///
    /// Extract the underlying contract, closurize it and wrap it back as a flat type (an opaque
    /// type defined by a custom contract).
    fn closurize(self, env: &mut Environment, with_env: Environment) -> Types {
        Types(AbsType::Flat(self.contract().closurize(env, with_env)))
    }
}

impl Closurizable for Contract {
    fn closurize(self, env: &mut Environment, with_env: Environment) -> Contract {
        Contract {
            types: self.types.closurize(env, with_env),
            label: self.label,
        }
    }
}
