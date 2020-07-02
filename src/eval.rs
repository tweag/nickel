use crate::identifier::Ident;
use crate::label::{Label, TyPath};
use crate::operation::{continuate_operation, OperationCont};
use crate::position::{ShowWithSource, SourceMapper};
use crate::stack::Stack;
use crate::term::{RichTerm, Term};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

pub type Environment = HashMap<Ident, (Rc<RefCell<Closure>>, IdentKind)>;
pub type CallStack = Vec<StackElem>;

#[derive(Debug, PartialEq, Clone)]
pub enum StackElem {
    App(Option<(usize, usize)>),
    Var(IdentKind, Ident, Option<(usize, usize)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IdentKind {
    Let(),
    Lam(),
    Record(),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub body: RichTerm,
    pub env: Environment,
}

impl Closure {
    pub fn atomic_closure(body: RichTerm) -> Closure {
        Closure {
            body,
            env: HashMap::new(),
        }
    }
}

type Span = (usize, usize);

#[derive(Debug, PartialEq)]
pub enum EvalError {
    /// A blame occurred
    BlameError(Label, Option<CallStack>),
    /// A mismatch between the expected type of an expression and its actual type
    TypeError(
        /* expected type */ String,
        /* operation */ String,
        RichTerm,
    ),
    /// A term which is not a function is applied to an argument
    NotAFunc(
        /* term */ RichTerm,
        /* arg */ RichTerm,
        /* app position */ Option<Span>,
    ),
    /// Access (or another record operation requiring the existence of a field) on a missing field
    FieldMissing(
        /* field identifier */ String,
        /* operator */ String,
        RichTerm,
        Option<Span>,
    ),
    /// Too few args were provided to a builtin function
    NotEnoughArgs(
        /* required arg count */ usize,
        /* primitive */ String,
        Option<Span>,
    ),
    MergeIncompatibleArgs(
        /* left operand */ RichTerm,
        /* right operand */ RichTerm,
        /* original merge */ Option<(usize, usize)>,
    ),
    Other(String, Option<Span>),
}

impl ShowWithSource for CallStack {
    fn show_append(&self, s: &mut String, mapper: &SourceMapper) {
        self.into_iter().rev().for_each(|e| {
            match e {
                // StackElem::App(Some((_l, _r))) => {
                //     // I'm not sure this App stack is really useful,
                //     // will leave it hanging for now
                //     //
                //     // if let Some((linef, colf)) = self.get_line_and_col(l) {
                //     //     s.push_str(&format!(
                //     //         "    Applied to a term on line: {} col: {}\n",
                //     //         linef, colf
                //     //     ));
                //     // }
                // }
                StackElem::Var(IdentKind::Let(), Ident(x), Some((left, _))) => {
                    if let Some(left) = mapper.map_pos(*left) {
                        s.push_str(&format!("On a call to {} at ", x));
                        left.show_append(s, mapper);
                        s.push('\n');
                    }
                }
                StackElem::Var(IdentKind::Lam(), Ident(x), Some((left, _))) => {
                    if let Some(left) = mapper.map_pos(*left) {
                        s.push_str(&format!("  Bound to {} at ", x));
                        left.show_append(s, mapper);
                        s.push('\n');
                    }
                }
                _ => {}
            }
        });
    }
}

impl EvalError {
    fn append_span(s: &mut String, mapper: &SourceMapper, span_opt: &Option<Span>) {
        span_opt
            .and_then(|(left, right)| mapper.map_span(left, right))
            .map(|span| span.show_append(s, mapper))
            .or_else(|| Some(s.push_str("<unknown location>")));
    }

    fn hding_span(s: &mut String, mapper: &SourceMapper, span_opt: &Option<Span>) {
        s.push_str("At ");
        EvalError::append_span(s, mapper, span_opt);
        s.push_str(": ");
    }
}

impl ShowWithSource for EvalError {
    fn show_append(&self, s: &mut String, mapper: &SourceMapper) {
        s.push_str("Error: ");
        match self {
            EvalError::BlameError(l, cs_opt) => {
                s.push_str("reached a blame label, some cast went terribly wrong\n");
                s.push_str("  Tag: ");
                s.push_str(&l.tag);
                s.push('\n');

                if let Some(span) = mapper.map_span(l.l, l.r) {
                    s.push_str("  At ");
                    span.show_append(s, mapper);
                }

                s.push_str(&format!("\n  Polarity: {}\n", l.polarity));
                if l.polarity {
                    s.push_str("  The blame is on the value (positive blame)\n");
                } else {
                    s.push_str("  The blame is on the context (negative blame)\n");
                }
                if l.path != TyPath::Nil() {
                    l.path.show_append(s, mapper);
                    s.push('\n');
                }

                if let Some(cs) = cs_opt {
                    s.push_str("\nCallstack:\n=========\n");
                    cs.show_append(s, mapper);
                }
            }
            EvalError::TypeError(expd, msg, t) => {
                EvalError::hding_span(s, mapper, &t.pos);

                s.push_str(&format!(
                    "expected {}, got {} ({})",
                    expd,
                    t.term.type_of().unwrap_or(String::from("<unevaluated>")),
                    msg
                ));
            }
            EvalError::NotAFunc(t, arg, pos_opt) => {
                EvalError::hding_span(s, mapper, &pos_opt);

                s.push_str(&format!("tried to apply {} (from ", t.term.shallow_repr()));
                EvalError::append_span(s, mapper, &t.pos);
                s.push_str(&format!(") to {} (from ", arg.term.shallow_repr()));
                EvalError::append_span(s, mapper, &arg.pos);
                s.push_str("), but this is not a function.")
            }
            EvalError::FieldMissing(field, op, t, span_opt) => {
                EvalError::hding_span(s, mapper, span_opt);

                s.push_str(&format!(
                    "field \"{}\" is required by {}, but was not found in the record at ",
                    field, op
                ));
                EvalError::append_span(s, mapper, &t.pos);
            }
            EvalError::NotEnoughArgs(count, op, span_opt) => {
                EvalError::hding_span(s, mapper, span_opt);

                s.push_str(&format!(
                    "{} expects {} argument(s), but not enough were provided.",
                    op, count
                ));
            }
            EvalError::MergeIncompatibleArgs(t1, t2, span_opt) => {
                EvalError::hding_span(s, mapper, span_opt);

                let RichTerm {
                    term: t1,
                    pos: pos1,
                } = t1;
                let RichTerm {
                    term: t2,
                    pos: pos2,
                } = t2;

                s.push_str(&format!("could not merge {} (from ", (*t1).shallow_repr()));
                EvalError::append_span(s, mapper, pos1);
                s.push_str(&format!(") with {} (from ", (*t2).shallow_repr()));
                EvalError::append_span(s, mapper, pos2);
                s.push(')');
            }
            EvalError::Other(msg, span_opt) => {
                EvalError::hding_span(s, mapper, span_opt);
                s.push_str(msg);
            }
        }
    }
}

fn is_value(_term: &Term) -> bool {
    false
}

pub fn eval(t0: RichTerm) -> Result<Term, EvalError> {
    let mut clos = Closure::atomic_closure(t0);
    let mut call_stack = CallStack::new();
    let mut stack = Stack::new();

    loop {
        let Closure {
            body: RichTerm {
                term: boxed_term,
                pos,
            },
            mut env,
        } = clos;
        let term = *boxed_term;
        clos = match term {
            // Var
            Term::Var(x) => {
                let (thunk, id_kind) = env
                    .remove(&x)
                    .unwrap_or_else(|| panic!("Unbound variable {:?}", x));
                std::mem::drop(env); // thunk may be a 1RC pointer
                if !is_value(&thunk.borrow().body.term) {
                    stack.push_thunk(Rc::downgrade(&thunk));
                }
                call_stack.push(StackElem::Var(id_kind, x, pos));
                match Rc::try_unwrap(thunk) {
                    Ok(c) => {
                        // thunk was the only strong ref to the closure
                        c.into_inner()
                    }
                    Err(rc) => {
                        // We need to clone it, there are other strong refs
                        rc.borrow().clone()
                    }
                }
            }
            // App
            Term::App(t1, t2) => {
                stack.push_arg(
                    Closure {
                        body: t2,
                        env: env.clone(),
                    },
                    pos,
                );
                Closure { body: t1, env }
            }
            // Let
            Term::Let(x, s, t) => {
                let thunk = Rc::new(RefCell::new(Closure {
                    body: s,
                    env: env.clone(),
                }));
                env.insert(x, (Rc::clone(&thunk), IdentKind::Let()));
                Closure { body: t, env }
            }
            // Unary Operation
            Term::Op1(op, t) => {
                let op = op.map(|t| Closure {
                    body: t,
                    env: env.clone(),
                });

                stack.push_op_cont(OperationCont::Op1(op), call_stack.len(), pos);
                Closure { body: t, env }
            }
            // Binary Operation
            Term::Op2(op, fst, snd) => {
                let op = op.map(|t| Closure {
                    body: t,
                    env: env.clone(),
                });

                stack.push_op_cont(
                    OperationCont::Op2First(
                        op,
                        Closure {
                            body: snd,
                            env: env.clone(),
                        },
                    ),
                    call_stack.len(),
                    pos,
                );
                Closure { body: fst, env }
            }
            // Promise and Assume
            Term::Promise(ty, l, t) | Term::Assume(ty, l, t) => {
                stack.push_arg(
                    Closure {
                        body: t,
                        env: env.clone(),
                    },
                    None,
                );
                stack.push_arg(Closure::atomic_closure(RichTerm::new(Term::Lbl(l))), None);
                Closure {
                    body: ty.contract(),
                    env,
                }
            }
            // Continuate Operation
            // Update
            _ if 0 < stack.count_thunks() || 0 < stack.count_conts() => {
                clos = Closure {
                    body: RichTerm {
                        term: Box::new(term),
                        pos,
                    },
                    env,
                };
                if 0 < stack.count_thunks() {
                    while let Some(thunk) = stack.pop_thunk() {
                        if let Some(safe_thunk) = Weak::upgrade(&thunk) {
                            *safe_thunk.borrow_mut() = clos.clone();
                        }
                    }
                    clos
                } else {
                    let cont_result = continuate_operation(clos, &mut stack, &mut call_stack);

                    if let Err(EvalError::BlameError(l, _)) = cont_result {
                        return Err(EvalError::BlameError(l, Some(call_stack)));
                    }
                    cont_result?
                }
            }
            // Call
            Term::Fun(x, t) => {
                if 0 < stack.count_args() {
                    let (arg, pos) = stack.pop_arg().expect("Condition already checked.");
                    call_stack.push(StackElem::App(pos));
                    let thunk = Rc::new(RefCell::new(arg));
                    env.insert(x, (thunk, IdentKind::Lam()));
                    Closure { body: t, env }
                } else {
                    return Ok(Term::Fun(x, t));
                }
            }

            t => {
                if 0 < stack.count_args() {
                    let (arg, pos_app) = stack.pop_arg().expect("Condition already checked.");
                    return Err(EvalError::NotAFunc(
                        RichTerm {
                            term: Box::new(t),
                            pos,
                        },
                        arg.body,
                        pos_app,
                    ));
                } else {
                    return Ok(t);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::label::TyPath;
    use crate::term::UnaryOp;

    #[test]
    fn identity_over_values() {
        let num = Term::Num(45.3);
        assert_eq!(Ok(num.clone()), eval(num.into()));

        let boolean = Term::Bool(true);
        assert_eq!(Ok(boolean.clone()), eval(boolean.into()));

        let lambda = Term::Fun(
            Ident("x".to_string()),
            RichTerm::app(RichTerm::var("x".into()), RichTerm::var("x".into())),
        );
        assert_eq!(Ok(lambda.clone()), eval(lambda.into()));
    }

    #[test]
    fn blame_panics() {
        let label = Label {
            tag: "testing".to_string(),
            l: 0,
            r: 1,
            polarity: false,
            path: TyPath::Nil(),
        };
        if let Err(EvalError::BlameError(l, _)) =
            eval(Term::Op1(UnaryOp::Blame(), Term::Lbl(label.clone()).into()).into())
        {
            assert_eq!(l, label);
        } else {
            panic!("This evaluation should've returned a BlameError!");
        }
    }

    #[test]
    #[should_panic]
    fn lone_var_panics() {
        eval(RichTerm::var("unbound".into())).unwrap();
    }

    #[test]
    fn only_fun_are_applicable() {
        eval(RichTerm::app(Term::Bool(true).into(), Term::Num(45.).into()).into()).unwrap_err();
    }

    #[test]
    fn simple_app() {
        let t = RichTerm::app(
            Term::Fun(Ident("x".to_string()), RichTerm::var("x".into())).into(),
            Term::Num(5.0).into(),
        );

        assert_eq!(Ok(Term::Num(5.0)), eval(t));
    }

    #[test]
    fn simple_let() {
        let t = RichTerm::let_in("x", Term::Num(5.0).into(), RichTerm::var("x".into()));

        assert_eq!(Ok(Term::Num(5.0)), eval(t));
    }

    #[test]
    fn simple_ite() {
        let t = RichTerm::ite(
            Term::Bool(true).into(),
            Term::Num(5.0).into(),
            Term::Bool(false).into(),
        );

        assert_eq!(Ok(Term::Num(5.0)), eval(t));
    }

    #[test]
    fn simple_plus() {
        let t = RichTerm::plus(Term::Num(5.0).into(), Term::Num(7.5).into());

        assert_eq!(Ok(Term::Num(12.5)), eval(t));
    }

    #[test]
    fn simple_is_zero() {
        let t = Term::Op1(UnaryOp::IsZero(), Term::Num(7.0).into()).into();

        assert_eq!(Ok(Term::Bool(false)), eval(t));
    }

    #[test]
    fn asking_for_various_types() {
        let num = Term::Op1(UnaryOp::IsNum(), Term::Num(45.3).into()).into();
        assert_eq!(Ok(Term::Bool(true)), eval(num));

        let boolean = Term::Op1(UnaryOp::IsBool(), Term::Bool(true).into()).into();
        assert_eq!(Ok(Term::Bool(true)), eval(boolean));

        let lambda = Term::Op1(
            UnaryOp::IsFun(),
            Term::Fun(
                Ident("x".to_string()),
                RichTerm::app(RichTerm::var("x".into()), RichTerm::var("x".into())),
            )
            .into(),
        )
        .into();
        assert_eq!(Ok(Term::Bool(true)), eval(lambda));
    }
}
