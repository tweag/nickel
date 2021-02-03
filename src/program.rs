//! Program handling, from file reading to evaluation.
//!
//! A program is Nickel source code loaded from an input. This module offers an interface to load a
//! program source, parse it, evaluate it and report errors.
//!
//! # Standard library
//!
//! Some essential functions required for evaluation, such as builtin contracts, are written in
//! pure Nickel. Standard library files must be record literals:
//!
//! ```
//! {
//!     val1 = ...
//!     val2 = ...
//! }
//! ```
//!
//! These .ncl file are not actually distributed as files, instead they are embedded, as plain
//! text, in the Nickel executable. The embedding is done by way of the [stdlib
//! module](../stdlib/index.html), which exposes the standard library files as strings. The
//! embedded strings are then parsed by the functions in this module (see
//! [`mk_global_env`](./struct.Program.html#method.mk_global_env)).  Each such value is added to
//! the global environment before the evaluation of the program.
use crate::cache::*;
use crate::error::{Error, ParseError, ToDiagnostic};
use crate::identifier::Ident;
use crate::parser::lexer::Lexer;
use crate::term::{RichTerm, Term};
use crate::{eval, parser};
use codespan::FileId;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::ffi::OsString;
use std::io::{self, Read};
use std::result::Result;

/// A Nickel program.
///
/// Manage a file database, which stores the original source code of the program and eventually the
/// code of imported expressions, and a dictionary which stores corresponding parsed terms.
pub struct Program {
    /// The id of the program source in the file database.
    main_id: FileId,
    /// The cache holding the sources and parsed terms of the main source as well as imports.
    cache: Cache,
}

impl Program {
    /// Create a program by reading it from the standard input.
    pub fn new_from_stdin() -> std::io::Result<Program> {
        Program::new_from_source(io::stdin(), "<stdin>")
    }

    pub fn new_from_file(path: impl Into<OsString>) -> std::io::Result<Program> {
        let mut cache = Cache::new();
        let main_id = cache.add_file(path)?;

        Ok(Program { main_id, cache })
    }

    /// Create a program by reading it from a generic source.
    pub fn new_from_source<T, S>(source: T, source_name: S) -> std::io::Result<Program>
    where
        T: Read,
        S: Into<OsString> + Clone,
    {
        let mut cache = Cache::new();
        let main_id = cache.add_source(source_name, source)?;

        Ok(Program { main_id, cache })
    }

    /// Retrieve the parsed term and typecheck it, and generate a fresh global environment. Return
    /// both.
    fn prepare_eval(&mut self) -> Result<(RichTerm, eval::Environment), Error> {
        self.cache.prepare_stdlib()?;
        let global_env = self
            .cache
            .mk_global_env()
            .expect("program::prepare_eval(): expected event to be ready");
        Ok((
            self.cache.prepare_nocache(self.main_id, &global_env)?,
            global_env,
        ))
    }

    /// Parse if necessary, typecheck and then evaluate the program.
    pub fn eval(&mut self) -> Result<Term, Error> {
        let (t, global_env) = self.prepare_eval()?;
        eval::eval(t, &global_env, &mut self.cache).map_err(|e| e.into())
    }

    /// Same as `eval`, but proceeds to a full evaluation.
    pub fn eval_full(&mut self) -> Result<Term, Error> {
        let (t, global_env) = self.prepare_eval()?;
        eval::eval_full(t, &global_env, &mut self.cache).map_err(|e| e.into())
    }

    /// Wrapper for [`query`](./fn.query.html).
    pub fn query(&mut self, path: Option<String>) -> Result<Term, Error> {
        self.cache.prepare_stdlib()?;
        let global_env = self
            .cache
            .mk_global_env()
            .expect("program::prepare_eval(): expected event to be ready");
        query(&mut self.cache, self.main_id, &global_env, path)
    }

    /// Load, parse, and typecheck the program and the standard library, if not already done.
    pub fn typecheck(&mut self) -> Result<(), Error> {
        self.cache.parse(self.main_id)?;
        self.cache.load_stdlib()?;
        self.cache.typecheck_stdlib().map_err(|err| err.unwrap_error("program::typecheck(): stdlib has been loaded but was not found in cache on typechecking"))?;
        let global_env = self.cache.mk_global_env().expect("program::typecheck(): stdlib has been loaded but was not found in cache on mk_global_env()");
        self.cache
            .typecheck(self.main_id, &global_env)
            .map_err(|cache_err| {
                cache_err.unwrap_error("program::typecheck(): expected source to be parsed")
            })?;
        Ok(())
    }

    /// Wrapper for [`report`](./fn.report.html).
    pub fn report<E>(&mut self, error: E)
    where
        E: ToDiagnostic<FileId>,
    {
        report(&mut self.cache, error)
    }
}

/// Query the metadata of a path of a term in the cache.
///
/// The path is a list of dot separated identifiers. For example, querying `{a = {b  = ..}}` with
/// path `a.b` will return a "weak" (see below) evaluation of `b`.
///
/// "Weak" means that as opposed to normal evaluation, it does not try to unwrap the content of a
/// metavalue: the evaluation stops as soon as a metavalue is encountered, although the potential
/// term inside the meta-value is forced, so that the concrete value of the field may also be
/// reported when present.
//TODO: more robust implementation than `let x = (y.path) in %seq% x x`, with respect to e.g.
//error message in case of syntax error or missing file.
//TODO: also gather type information, such that `query a.b.c <<< '{ ... } : {a: {b: {c: Num}}}`
//would additionally report `type: Num` for example.
//TODO: not sure where this should go. It seems to embed too much logic to be in `Cache`, but is
//common to both `Program` and `REPL`. Leaving it here as a stand-alone function for now
pub fn query(
    cache: &mut Cache,
    file_id: FileId,
    global_env: &eval::Environment,
    path: Option<String>,
) -> Result<Term, Error> {
    use std::cell::RefCell;
    use std::rc::Rc;

    cache.prepare(file_id, global_env)?;

    let t = if let Some(p) = path {
        // Parsing `y.path`. We `seq` it to force the evaluation of the underlying value,
        // which can be then showed to the user. The newline gives better messages in case of
        // errors.
        let source = format!("let x = (y.{})\n in %seq% x x", p);
        let query_file_id = cache.add_tmp("<query>", source.clone());
        let new_term = parser::grammar::TermParser::new()
            .parse(query_file_id, Lexer::new(&source))
            .map_err(|err| ParseError::from_lalrpop(err, query_file_id))?;

        // Substituting `y` for `t`
        let mut env = eval::Environment::new();
        let closure = eval::Closure {
            body: cache.get_owned(file_id).unwrap(),
            env: eval::Environment::new(),
        };
        env.insert(
            Ident::from("y"),
            (Rc::new(RefCell::new(closure)), eval::IdentKind::Let()),
        );

        //TODO: why passing an empty global environment?
        eval::subst(new_term, &eval::Environment::new(), &env)
    } else {
        cache.get_owned(file_id).unwrap()
    };

    Ok(eval::eval_meta(t, &global_env, cache)?)
}

/// Pretty-print an error.
///
/// This function is located here in `Program` because errors need a reference to `files` in
/// order to produce a diagnostic (see [`label_alt`](../error/fn.label_alt.html)).
//TODO: not sure where this should go. It seems to embed too much logic to be in `Cache`, but is
//common to both `Program` and `REPL`. Leaving it here as a stand-alone function for now
pub fn report<E>(cache: &mut Cache, error: E)
where
    E: ToDiagnostic<FileId>,
{
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let contracts_id = cache.id_of("<stdlib/contracts.ncl>");
    let diagnostics = error.to_diagnostic(cache.files_mut(), contracts_id);

    let result = diagnostics.iter().try_for_each(|d| {
        codespan_reporting::term::emit(&mut writer.lock(), &config, cache.files_mut(), &d)
    });
    match result {
        Ok(()) => (),
        Err(err) => panic!(
            "Program::report: could not print an error on stderr: {}",
            err
        ),
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::EvalError;
    use crate::identifier::Ident;
    use crate::parser::{grammar, lexer};
    use assert_matches::assert_matches;
    use codespan::Files;
    use std::io::Cursor;

    fn parse(s: &str) -> Option<RichTerm> {
        let id = Files::new().add("<test>", String::from(s));

        grammar::TermParser::new()
            .parse(id, lexer::Lexer::new(&s))
            .map(|mut t| {
                t.clean_pos();
                t
            })
            .map_err(|err| println!("{:?}", err))
            .ok()
    }

    fn eval_string(s: &str) -> Result<Term, Error> {
        let src = Cursor::new(s);

        let mut p = Program::new_from_source(src, "<test>").map_err(|io_err| {
            Error::EvalError(EvalError::Other(format!("IO error: {}", io_err), None))
        })?;
        p.eval()
    }

    fn eval_string_full(s: &str) -> Result<Term, Error> {
        let src = Cursor::new(s);

        let mut p = Program::new_from_source(src, "<test>").map_err(|io_err| {
            Error::EvalError(EvalError::Other(format!("IO error: {}", io_err), None))
        })?;
        p.eval_full()
    }

    /// Assert if a given Nickel expression evaluates to a record, given as a vector of bindings
    /// Records are lazy, thus we need to force the evaluation of each field. Since `merge`
    /// replaces subterms with dummy fresh variables, we have to re-evaluate the whole expression
    /// to verify each field
    fn assert_eval_to_record(s: &str, res: Vec<(&str, Term)>) {
        let mut term = eval_string(s);

        //Check that the record does not contain extra keys
        if let Ok(Term::Record(ref mut m)) = term {
            for (i, t) in res {
                m.remove(&Ident::from(i))
                    .unwrap_or_else(|| panic!(format!("Could not find field {} in result", i)));

                let proj = format!("({}).{}", s, i);
                if let Ok(proj_res) = eval_string(&proj) {
                    assert_eq!(proj_res, t);
                } else {
                    panic!(format!("evaluation of the projection on {} failed", i));
                }
            }

            assert!(m.is_empty());
        } else if let Err(err) = term {
            panic!("evaluation failed: {:?}", err);
        }
    }

    #[test]
    fn function_app() {
        let res = eval_string("(fun h => h) 3");

        assert_eq!(Ok(Term::Num(3.0)), res);
    }

    #[test]
    fn let_binding() {
        let res = eval_string("let f = fun f => fun y => f (f y) in f (fun h => h) 3");

        assert_eq!(Ok(Term::Num(3.0)), res);
    }

    #[test]
    fn plus() {
        let res = eval_string("34 + (if true then 2 else 222)");

        assert_eq!(Ok(Term::Num(36.0)), res);
    }

    #[test]
    fn dynamic_if() {
        let res =
            eval_string("let g  = fun x => if x then 0 else false in g ((fun x => true) 23 )");

        assert_eq!(Ok(Term::Num(0.0)), res);
    }

    #[test]
    fn parse_error() {
        let res = eval_string("let g  = funky x => x in g true");

        if let Ok(_) = res {
            panic!("This test should have returned Err()!");
        }
    }

    #[test]
    fn simple_type_check() {
        let res = eval_string("let x = 5 in if %isNum% x then true else 1");

        assert_eq!(Ok(Term::Bool(true)), res);
    }

    #[test]
    fn fixpoint() {
        let res = eval_string(
            "let Y = (fun f => (fun x => f (x x)) (fun x => f (x x))) in
let g = Y (fun g => (fun x => if x  then (g false) else 4)) in
g true",
        );

        assert_eq!(Ok(Term::Num(4.)), res);
    }

    #[test]
    fn type_contracts() {
        let res = eval_string(
            "let safePlus : Num -> Num -> Num = fun x => fun y => x + y in
            safePlus (54 : Num) (6 : Num)",
        );

        assert_eq!(Ok(Term::Num(60.)), res);
    }

    #[test]
    fn fibonacci() {
        let res = eval_string(
            "let Y = Assume(((Num -> Num) -> Num -> Num) -> Num -> Num, fun f => (fun x => f (x x)) (fun x => f (x x))) in
            let dec : Num -> Num = fun x => x + (-1) in
            let or : Bool -> Bool -> Bool = fun x => fun y => if x then x else y in

            let fibo : Num -> Num = Y (fun fibo =>
                (fun x => if or (x == 0) (dec x == 0) then 1 else (fibo (dec x)) + (fibo (dec (dec x))))) in
            let val : Num = 4 in
            fibo val",
        );

        assert_eq!(Ok(Term::Num(5.)), res);
    }

    #[test]
    fn promise_fail() {
        let res = eval_string("let bool = fun l t => if isBool t then t else blame l in 5 : Bool");

        if let Ok(_) = res {
            panic!("This expression should return an error!");
        }
    }

    #[test]
    fn flat_contract_fail() {
        let res = eval_string(
            "let alwaysTrue = fun l t => let boolT = Assume(Bool, t) in
    if boolT then boolT else blame l in
Assume(#alwaysTrue, false)
",
        );
        if let Ok(_) = res {
            panic!("This expression should return an error!");
        }
    }

    #[test]
    fn flat_higher_order_contract() {
        let res = eval_string(
            "let alwaysTrue = fun l t =>
              let boolT = Assume(Bool, t) in
              if boolT then boolT else %blame% l
            in
            let alwaysFalse = fun l t =>
              let boolT = Assume(Bool, t) in
              if boolT then %blame% l else boolT
            in
            let not = fun b => if b then false else true in
            Assume(#alwaysTrue -> #alwaysFalse, not ) true",
        );

        assert_eq!(Ok(Term::Bool(false)), res);
    }

    #[test]
    fn safe_id() {
        let res = eval_string(
            "let id = Assume(forall a. a -> a, fun x => x) in
            id false",
        );
        assert_eq!(Ok(Term::Bool(false)), res);
    }

    #[test]
    fn id_fail() {
        let res = eval_string(
            "let id = Assume(forall a. a -> a, fun x => false) in
            id false",
        );
        if let Ok(_) = res {
            panic!("This expression should return an error!");
        }
    }

    #[test]
    fn safe_higher_order() {
        let res = eval_string(
            "let to_bool = Assume(forall a. (a -> Bool) -> a -> Bool,
            fun f => fun x => f x) in
            to_bool (fun x => true) 4 ",
        );
        assert_eq!(Ok(Term::Bool(true)), res);
    }

    #[test]
    fn apply_twice() {
        let res = eval_string(
            "let twice = Assume(forall a. (a -> a) -> a -> a,
            fun f => fun x => f (f x)) in
            twice (fun x => x + 1) 3",
        );
        assert_eq!(Ok(Term::Num(5.)), res);
    }

    #[test]
    fn string_contracts() {
        let res = eval_string("Assume(Str, \"hello\")");
        assert_eq!(res, Ok(Term::Str("hello".to_string())));

        let res = eval_string("Assume(Str, \"hello\" ++ \" world!\")");
        assert_eq!(res, Ok(Term::Str("hello world!".to_string())));
    }

    #[test]
    fn enum_simple() {
        let res = eval_string("`foo : <foo, bar>");
        assert_eq!(res, Ok(Term::Enum(Ident("foo".to_string()))));

        let res = eval_string("`bar : forall r. <foo, bar | r>");
        assert_eq!(res, Ok(Term::Enum(Ident("bar".to_string()))));

        eval_string("`far : <foo, bar>").unwrap_err();
    }

    #[test]
    fn enum_complex() {
        let res = eval_string(
            "let f : forall r. <foo, bar | r> -> Num =
            fun x => switch { foo => 1, bar => 2, _ => 3, } x in
            f `bar",
        );
        assert_eq!(res, Ok(Term::Num(2.)));

        let res = eval_string(
            "let f : forall r. <foo, bar | r> -> Num =
            fun x => switch { foo => 1, bar => 2, _ => 3, } x in
            f `boo",
        );
        assert_eq!(res, Ok(Term::Num(3.)));

        eval_string(
            "let f : <foo, bar> -> Num =
            fun x => switch { foo => 1, bar => 2, } x in
            f `boo",
        )
        .unwrap_err();

        // This test checks that the terms of a switch are closured
        assert_eq!(
            eval_string(
                "let x = 3 in
                switch { foo => 1, _ => x, } (3 + 2)"
            ),
            Ok(Term::Num(3.))
        );
    }

    #[test]
    fn row_types() {
        eval_string("Assume(< >, 123)").unwrap_err();
    }

    #[test]
    fn records_accessing() {
        assert_eq!(
            eval_string("({ foo = 3; bar = true; }).bar"),
            Ok(Term::Bool(true)),
        );

        assert_eq!(
            eval_string("({ $(if true then \"foo\" else \"bar\") = false; bar = true; }).foo"),
            Ok(Term::Bool(false)),
        );

        assert_eq!(
            eval_string("({ foo = 3; bar = true; }).$(\"bar\")"),
            Ok(Term::Bool(true)),
        );

        assert_eq!(
            eval_string(
                "({ $(if true then \"foo\" else \"bar\") = false; bar = true; }).$(\"foo\")"
            ),
            Ok(Term::Bool(false)),
        );

        eval_string("({ $(if false then \"foo\" else \"bar\") = false; bar = true; }).foo")
            .unwrap_err();
    }

    #[test]
    fn records_prims() {
        assert_eq!(
            eval_string("%hasField% \"foo\" { foo = 1; bar = 2; }"),
            Ok(Term::Bool(true))
        );
        assert_eq!(
            eval_string("%hasField% \"fop\" { foo = 1; bar = 2; }"),
            Ok(Term::Bool(false))
        );

        assert_eq!(
            eval_string(
                "(%recordMap% { foo = 1; bar = \"it's lazy\"; } (fun y => fun x => x + 1)).foo"
            ),
            Ok(Term::Num(2.)),
        );
        assert_eq!(
            eval_string(
                "let r = %recordMap%
                    { foo = 1; bar = \"it's lazy\"; }
                    (fun y x => if %isNum% x then x + 1 else 0)
                in
                (r.foo) + (r.bar)"
            ),
            Ok(Term::Num(2.)),
        );

        assert_eq!(
            eval_string("%hasField% \"foo\" ( { foo = 2; bar = 3; }-$(\"foo\"))"),
            Ok(Term::Bool(false))
        );

        assert_eq!(
            eval_string("%hasField% \"foo\" ( { bar = 3; }$[\"foo\" = 1])"),
            Ok(Term::Bool(true))
        );
        assert_eq!(
            eval_string("( { bar = 3; }$[\"foo\" = true]).foo"),
            Ok(Term::Bool(true))
        );
    }

    /// This currently do not check that subexpressions are actually forced,
    /// just that the evaluation succeeds
    #[test]
    fn seq_expressions() {
        assert_eq!(eval_string("%seq% 1 true"), Ok(Term::Bool(true)));
        assert_eq!(
            eval_string("let x = (1 + 1) in %seq% x x"),
            Ok(Term::Num(2.0))
        );

        assert_eq!(
            eval_string("let r = {a=(1 + 1);} in %deepSeq% r (r.a)"),
            Ok(Term::Num(2.0))
        );
        assert_eq!(
            eval_string("let r = {a=(1 + 1);b=(\"a\" ++ \"b\");} in %deepSeq% r (r.b)"),
            Ok(Term::Str(String::from("ab")))
        );
        assert_eq!(
            eval_string("let r = {a={b=(1 + 1);};} in %deepSeq% r ((r.a).b)"),
            Ok(Term::Num(2.0))
        );

        assert_eq!(
            eval_string(
                "let inj = fun x => {b=(x + 2);} in
                let cat = fun x => fun y => x ++ y in
                let r = {a=(inj 1);b=(cat \"a\" \"b\");} in %deepSeq% r ((r.a).b)"
            ),
            Ok(Term::Num(3.0))
        )
    }

    #[test]
    fn lists() {
        assert_eq!(eval_string("%elemAt% [1,2,3] 1"), Ok(Term::Num(2.0)));
        assert_eq!(
            eval_string("%elemAt% (%map% [1,2,3] (fun x => x + 1)) 1"),
            Ok(Term::Num(3.0))
        );

        eval_string("%elemAt% [1,2,3] (-1)").unwrap_err();
        eval_string("%elemAt% [1,2,3] 4").unwrap_err();

        assert_eq!(eval_string("%length% []"), Ok(Term::Num(0.0)));
        assert_eq!(eval_string("%length% [1,2,3]"), Ok(Term::Num(3.0)));

        assert_eq!(
            eval_string("%length% ([] @ [1,2] @ [3,4] @ [])"),
            Ok(Term::Num(4.0))
        );
        // Test case added after https://github.com/tweag/nickel/issues/154
        assert_eq!(
            eval_string("let x = 1 in let l = [x] @ [2] in %head% l"),
            Ok(Term::Num(1.0))
        );

        assert_eq!(
            eval_string("%head% [\"a\",\"b\",\"c\"]"),
            Ok(Term::Str(String::from("a")))
        );
        eval_string("%head% []").unwrap_err();

        assert_eq!(
            eval_string("%length% (%tail% [true,false,1])"),
            Ok(Term::Num(2.0))
        );
        eval_string("%tail% []").unwrap_err();

        assert_eq!(
            eval_string(
                "let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
                let foldr_ =
                    fun self => fun f => fun acc => fun l =>
                        if %length% l == 0 then acc
                        else
                            let h = %head% l in
                            let t = %tail% l in
                            let next_acc = self f acc t in
                            f next_acc h
                in
                let foldr = Y foldr_ in
                let and : Bool -> Bool -> Bool =
                    fun x => fun y =>
                        if x then
                            if y then true else false
                        else false
                in
                let all = fun pred => fun l => foldr and true (%map% l pred) in
                let isZ = fun x => x == 0 in
                all isZ [0, 0, 0, 1]"
            ),
            Ok(Term::Bool(false))
        );
    }

    #[test]
    fn merge_record_simple() {
        assert_eval_to_record(
            "{a=1;} & {b=true;}",
            vec![("a", Term::Num(1.0)), ("b", Term::Bool(true))],
        );
    }

    #[test]
    #[should_panic]
    fn merge_record_failure() {
        eval_string("({a=1} & {a=2}).a").unwrap();
    }

    #[test]
    fn merge_record_intersection() {
        assert_eval_to_record(
            "{a=1;b=2;} & {b=2;c=3;}",
            vec![
                ("a", Term::Num(1.0)),
                ("b", Term::Num(2.0)),
                ("c", Term::Num(3.0)),
            ],
        );
    }

    #[test]
    fn merge_record_nested() {
        assert_eval_to_record(
            "({a={b=1;};} & {a={c=true;};}).a ",
            vec![("b", Term::Num(1.0)), ("c", Term::Bool(true))],
        );
    }

    #[test]
    fn merge_record_complex() {
        assert_eval_to_record(
            "let rec1 = {a=false;b=(if true then (1 + 1) else (2 + 0)); c=(((fun x => x) (fun y => y)) 2);} in
             let rec2 = {b=(((fun x => x) (fun y => y)) 2); c=(if true then (1 + 1) else (2 + 0)); d=true;} in
             rec1 & rec2",
             vec![("a", Term::Bool(false)), ("b", Term::Num(2.0)), ("c", Term::Num(2.0)), ("d", Term::Bool(true))]
         );
    }

    #[test]
    fn merge_record_with_env() {
        assert_eq!(
            eval_string("((fun y => ((fun x => {a=y}) 1) & ({b=false})) 2).a"),
            Ok(Term::Num(2.0))
        );
    }

    #[test]
    fn merge_record_with_env_nested() {
        assert_eq!(
            eval_string(
                "let rec = {b={c=10}} & ((fun x => {a=x; b={c=x}}) 10) in
                         rec.b.c"
            ),
            Ok(Term::Num(10.0))
        );
    }

    #[test]
    #[should_panic]
    fn enriched_terms_contract_default_fail() {
        eval_string("ContractDefault(Num, true)").unwrap();
    }

    #[test]
    fn enriched_terms_contract_default() {
        assert_eq!(eval_string("ContractDefault(Num, 10)"), Ok(Term::Num(10.0)));
    }

    // Check the correct handling of the update of thunks containing enriched values (see issue
    // https://github.com/tweag/nickel/issues/123)
    #[test]
    fn enriched_terms_thunk_update() {
        assert_eq!(
            eval_string("let x = {a=(fun x => Default(1)) 1} in %seq% (x.a) ((x & {a=2}).a)"),
            Ok(Term::Num(2.0))
        );
    }

    #[test]
    fn merge_default() {
        assert_eval_to_record(
            "{a=2;} & {a=Default(0);b=Default(true);}",
            vec![("a", Term::Num(2.0)), ("b", Term::Bool(true))],
        );

        assert_eval_to_record(
            "({a=Default({x=1;});} & {a=Default({y=\"y\";});}).a",
            vec![("x", Term::Num(1.0)), ("y", Term::Str(String::from("y")))],
        );

        eval_string("({a=Default(1);} & {a=Default(2);}).a").unwrap_err();
    }

    #[test]
    fn merge_contract() {
        assert_eval_to_record(
            "{a=2;b=Contract(Bool);} & {a=Contract(Num);b=Default(true);}",
            vec![("a", Term::Num(2.0)), ("b", Term::Bool(true))],
        );

        eval_string("let r = {a=2;} & {a=Contract(Bool)} in r.a").unwrap_err();
    }

    #[test]
    fn merge_default_contract() {
        assert_eval_to_record(
            "{a=2;} & {a=ContractDefault(Num, 0);b=Default(true)}",
            vec![("a", Term::Num(2.0)), ("b", Term::Bool(true))],
        );

        assert_eval_to_record(
            "{a=2} & {a=Contract(Num)} & {a=Default(3)}",
            vec![("a", Term::Num(2.0))],
        );

        assert_eq!(
            eval_string("({a=2;} & {b=Contract(Num)} & {b=Default(3);}).b"),
            Ok(Term::Num(3.0)),
        );

        assert_eq!(
            eval_string("({a=Default(1)} & {b=Contract(Num)} & {a=Default(1)}).a"),
            Ok(Term::Num(1.0)),
        );

        eval_string("({a=2;} & {b=Contract(Num)} & {b=Default(true)}).b").unwrap_err();
    }

    fn make_composed_contract(value: &str) -> Result<Term, Error> {
        let s = format!(
            "let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
             let dec = fun x => x + (-1) in
             let or = fun x => fun y => if x then x else y in
             let isEven_ = Y (fun f =>
                 (fun x =>
                     if x == 0 then true
                     else (
                         if dec x == 0 then false
                         else (f (dec (dec x)))
                     )
                 )
             ) in
             let isDivBy3_ = Y (fun f =>
                 (fun x =>
                     if x == 0 then true
                     else (
                         if or (dec (dec x) == 0) (dec x == 0) then false
                         else (f (dec (dec (dec x))))
                     )
                 )
             ) in
             let toCtr = fun f => fun l => fun x => (
               if (%isNum% x) then (
                 if (f x) then x else %blame% l)
               else %blame% l
             ) in
             let isEven = toCtr isEven_ in
             let isDivBy3 = toCtr isDivBy3_ in
             let composed = {{a=Contract(#isEven)}} & {{a=Contract(#isDivBy3)}} in
             ({{a={}}} & composed).a",
            value
        );

        eval_string(s.as_str())
    }

    #[test]
    fn merge_compose_contract() {
        assert_eq!(make_composed_contract("6"), Ok(Term::Num(6.0)));
        assert_eq!(make_composed_contract("12"), Ok(Term::Num(12.0)));

        make_composed_contract("1").unwrap_err();
        make_composed_contract("14").unwrap_err();
        make_composed_contract("27").unwrap_err();
        make_composed_contract("10").unwrap_err();
        make_composed_contract("35").unwrap_err();
    }

    /// Check that the environments of contracts are correctly saved and restored when merging. See
    /// issue [#117](https://github.com/tweag/nickel/issues/117).
    #[test]
    fn merge_contracts_with_env() {
        let definitions = "
            let ctr_num = let x = num in {a = Contract(#x)} in
            let ctr_id = let x = fun l x => x in {a = Contract(#x)} in
            let val = let x = 1 in {a = x} in
            let def = let x = 2 in {a = Default(x)} in
            let def2 = let x = (1 + 1) in {a = Default(x)} in
            ";

        // Build a term which recursively merges the elements of `elts` in the environment set by
        // `definitions`, and then access the `a` field
        let merge_elts = |elts: Vec<&str>| -> Result<Term, Error> {
            let term = elts.into_iter().fold(String::from("{}"), |term, op| {
                format!("({}) & ({})", op, term)
            });
            eval_string(&format!("{} ({}).a", definitions, term))
        };

        // contract/contract -> contract/value -> value/default
        assert_eq!(
            merge_elts(vec!["ctr_num", "ctr_id", "val", "def"]),
            Ok(Term::Num(1.0))
        );
        // default/value <- value/contract
        assert_eq!(merge_elts(vec!["def", "val & ctr_num"]), Ok(Term::Num(1.0)));
        // default/contract-> contract-default/contract-default <- contract/default
        assert_eq!(
            merge_elts(vec!["def & ctr_num", "ctr_id & def2"]),
            Ok(Term::Num(2.0))
        );
        // default/contract -> contract-default/contract -> contract-default/value
        assert_eq!(
            merge_elts(vec!["def", "ctr_num", "ctr_id", "val"]),
            Ok(Term::Num(1.0))
        );
        // default/contract -> contract-default/default
        assert_eq!(
            merge_elts(vec!["def", "ctr_num", "def2"]),
            Ok(Term::Num(2.0))
        );
        // value/contract-default <- contract/contract-default
        assert_eq!(merge_elts(vec!["val", "ctr_num & def"]), Ok(Term::Num(1.0)));
    }

    #[test]
    fn string_chunks() {
        fn assert_eval_str(term: &str, result: &str) {
            assert_eq!(eval_string(term), Ok(Term::Str(String::from(result))));
        }

        assert_eval_str(
            r#""simple #{"interp" ++ "olation"} here""#,
            "simple interpolation here",
        );
        assert_eval_str(r##""#{"alone"}""##, "alone");
        assert_eval_str(
            r##""nested #{ "#{(fun x => "#{x}") "expression"}" }""##,
            "nested expression",
        );
        assert_eval_str(
            r##""#{"some"}#{" " ++ "se" ++ "qu"}#{"#{"ence"}"}""##,
            "some sequence",
        );
        assert_eval_str(
            r##""nested #{ {str = {a = "braces"}.a}.str } !""##,
            "nested braces !",
        );
        assert_eval_str(
            r##"let x = "world" in "Hello, #{x}! Welcome in #{let y = "universe" in "the #{x}-#{y}"}""##,
            "Hello, world! Welcome in the world-universe",
        );

        match eval_string(r##""bad type #{1 + 1}""##) {
            Err(Error::EvalError(EvalError::TypeError(_, _, _, _))) => (),
            _ => assert!(false),
        };
    }

    // The chunk concatenation operator directly uses the stack to store its continuation. Check
    // that it does not mess up the stack.
    #[test]
    fn chunks_nested() {
        // Generate an non-empty evaluation context to evaluate chunks over a non-empty stack
        let with_context = |t| format!("let force = fun s => s ++ \"\" in force (force ({}))", t);

        fn assert_eval_str(term: &str, result: &str) {
            assert_eq!(eval_string(term), Ok(Term::Str(String::from(result))));
        }

        assert_eval_str(
            &with_context(r##""nested #{ {str = {a = "braces"}.a}.str } !""##),
            "nested braces !",
        );

        assert_eval_str(
            &with_context(
                r##"let x = "world" in "Hello, #{x}! Welcome in #{let y = "universe" in "the #{x}-#{y}"}""##,
            ),
            "Hello, world! Welcome in the world-universe",
        );
    }

    #[test]
    fn recursive_records() {
        assert_eq!(
            eval_string("{a = 1; b = a + 1; c = b + a}.c"),
            Ok(Term::Num(3.0))
        );
        assert_eq!(
            eval_string("{f = fun x y => if x == 0 then y else f (x + (-1)) (y + 1)}.f 5 5"),
            Ok(Term::Num(10.0))
        );
        assert_eq!(
            eval_string("
                let with_res = fun res => { f = fun x => if x == 0 then res else g x; g = fun y => f (y + (-1)) }.f 10 in
                with_res \"done\""),
            Ok(Term::Str(String::from("done")))
        );
    }

    /// Assert that the polymorphic equality between two terms, given as strings, evaluates to the
    /// term `true`.
    ///
    /// This is made as a macro such that a failure points directly at the failing test line.  With
    /// a standard function, a failing assertion would always point at this function's body,
    /// instead of the callsite.
    macro_rules! assert_peq {
        ($t1:expr, $t2:expr) => {
            assert_eq!(
                eval_string(&format!("({}) == ({})", $t1, $t2)),
                Ok(Term::Bool(true)),
            );
        };
    }

    /// Same as `assert_peq!`, but succeeds if terms are **not** equal.
    macro_rules! assert_npeq {
        ($t1:expr, $t2:expr) => {
            assert_eq!(
                eval_string(&format!("({}) == ({})", $t1, $t2)),
                Ok(Term::Bool(false)),
            );
        };
    }

    #[test]
    fn poly_eq() {
        assert_peq!("0", "0 + 0 + 0");
        assert_peq!("true", "if true then true else false");
        assert_peq!("\"a\" ++ \"b\" ++ \"c\"", "\"#{\"a\" ++ \"b\"}\" ++ \"c\"");
        assert_peq!("`Less", "`Less");
        assert_peq!("`small", "`small");

        assert_npeq!("1 + 1", "0");
        assert_npeq!("true", "if true then false else true");
        assert_npeq!("\"a\"", "\"a\" ++ \" \"");
        assert_npeq!("1", "true");
        assert_npeq!("\"1\"", "1");
        assert_npeq!("\"true\"", "true");
        assert_npeq!("`Less", "`small");
        assert_npeq!("`Less", "0");
        assert_npeq!("`Greater", "false");
    }

    #[test]
    fn poly_eq_lists() {
        assert_peq!("[]", "[]");
        assert_peq!("[(1 + 0), (1 + 1), (1 + 1 + 1)]", "[1, 2, 3]");
        assert_peq!(
            "[(1 + 0), (\"a\" ++ \"b\"), (if true then true else false)]",
            "[1, \"ab\", true]"
        );

        assert_peq!("[[[]]]", "[[[]]]");
        assert_peq!("[[1], [[2]]]", "[[2 + (-1)], [[1 + 1]]]");
        assert_peq!("[[true, false]]", "lists.flatten [[[true, false]]]");

        assert_npeq!("[]", "[1]");
        assert_npeq!("[]", "1");
        assert_npeq!("[]", "{}");
        assert_npeq!("[1, \"a\", true]", "[1, \"a\", false]");
        assert_npeq!("[[true]]", "[[[true]]]");
    }

    #[test]
    fn poly_eq_records() {
        assert_peq!("{}", "{}");
        assert_peq!("{}$[\"a\" = 1]$[\"b\" = true]", "{a = 1; b = true}");
        assert_peq!(
            "{a = 1 + 0; b = 1 + 1; c = 1 + 1 + 1}",
            "{ a = 1; b = 2; c = 3 }"
        );
        assert_peq!(
            "{ foo = 1 + 0; bar = \"a\" ++ \"b\"; baz = if true then true else false }",
            "{ foo = 1; bar = \"ab\"; baz = true}"
        );

        assert_peq!(
            "{}$[\"a\" = { a = { a = {} } }]",
            "{ a = { a = { a = {} } } }"
        );
        assert_peq!(
            "{ foo = { bar = 2 + (-1) }; baz = { foo = { bar = 1 + 1 } } }",
            "{ foo = { bar = 1 }; baz = { foo = { bar = 2 } } }"
        );

        assert_npeq!("{}", "{a = true}");
        assert_npeq!("{a = 1}", "{a = 2}");
        assert_npeq!("{ a = \"a\"; b = true }", "{ a = true; b = \"a\"}");
        assert_npeq!("{ a = { a = true } }", "{a = { a = { a = true } } }");
    }

    // Now that the equality operator directly uses the stack to store its continuation (see
    // https://github.com/tweag/nickel/pull/247), check that it correctly cleans the stack when
    // evaluating a subequality to `false`.
    #[test]
    fn poly_eq_nested() {
        // Generate an non-empty evaluation context to evaluate equalities over a non-empty stack
        let with_context = |t1, t2| {
            format!("let not = fun b => if b then true else false in not (not (not (not (({}) == ({})))))", t1, t2)
        };

        assert_peq!(
            with_context(
                "{a = 1 + 0; b = 1 + 1; c = 0; d = 0}",
                "{ a = 1; b = 3; c = 0; d = 0}"
            ),
            "false"
        );

        assert_peq!(
            with_context(
                "[[1,2,3,4], [1,0,3,4], [1,2,3,4], [1,2,3,4]]",
                "[[1,2,3,4], [1,2,3,4], [1,2,3,4], [1,2,3,4]]"
            ),
            "false"
        );
    }

    #[test]
    fn fields_of() {
        assert_peq!("%fieldsOf% {}", "[]");
        assert_peq!("%fieldsOf% {a = 1; b = 2; c = 3}", "[\"a\", \"b\", \"c\"]");
        assert_peq!("%fieldsOf% {aAa = 1; Zzz = 2;}", "[\"Zzz\", \"aAa\"]");
        assert_peq!(
            "%fieldsOf% {foo = {bar = 0}; baz = Default(true)}",
            "[\"baz\", \"foo\"]"
        );
    }

    #[test]
    fn arithmetic_expr() {
        assert_peq!("1+1", "2");
        assert_peq!("1-2+3-4", "-2");
        assert_peq!("2-3-4", "-5");
        assert_peq!("-1-2", "-3");
        assert_peq!("2*2 + 2*3 - 2*4", "2");
        assert_peq!("1/2 + 1/4 - 1/8", "0.625");
        assert_peq!("(10 + 1/4) % 3", "1.25");
        assert_peq!("10 + 1/4 % 3", "10.25");

        eval_string("1 + 1 / (1 - 1)").unwrap_err();
    }

    #[test]
    fn comparisons() {
        assert_peq!("1 < 1", "false");
        assert_peq!("1 <= 1", "true");
        assert_peq!("1 > 1", "false");
        assert_peq!("1 >= 1", "true");
        assert_peq!("1 + 1/2 > 1 + 1/4", "true");
        assert_peq!("1 + 1/2 < 1 + 1/4", "false");
        assert_peq!("1 + 1/2 + 1/8 > 1 + 1/4 + 1/4", "true");
        assert_peq!("1 + 1/2 + 1/8 < 1 + 1/4 + 1/4", "false");
        assert_peq!("-1 - 2 < 3 - 10", "false");
        assert_peq!("-1 - 2 > 3 - 10", "true");
        assert_peq!("-1*2 > 1*2", "false");
        assert_peq!("-1*2 < 1*2", "true");
        assert_peq!("1/4 + 1/4 - 1/4 + 1/4 <= 1/2", "true");
        assert_peq!("1/4 + 1/4 - 1/4 + 1/4 < 1/2", "false");
        assert_peq!("1/4 + 1/4 - 1/4 + 1/4 >= 1/2", "true");
        assert_peq!("1/4 + 1/4 - 1/4 + 1/4 < 1/2", "false");

        eval_string("1 < 2 < 3").unwrap_err();
        eval_string("1 < 2 > 3").unwrap_err();
        eval_string("\"a\" < 2").unwrap_err();
        eval_string("true <= []").unwrap_err();
        eval_string("\"a\" > \"b\"").unwrap_err();
        eval_string("\"a\" >= \"b\"").unwrap_err();
    }

    #[test]
    fn boolean_op() {
        assert_peq!("1+1>1 && %isStr% 0", "false");
        assert_peq!("-1 < -2 && %isFun% (fun x => x)", "false");
        assert_peq!(
            "%isNum% 0 && %isFun% (fun x => x) || 1 == 0 && true",
            "true"
        );
        assert_peq!(
            "%isNum% 0 && %isFun% false || 1 == 0 && false || 1 > 2 && 2 < 1",
            "false"
        );
        assert_peq!(
            "!(%isNum% true) && !(%isFun% 0) && !(%isBool% \"a\")",
            "true"
        );
        assert_peq!(
            "!(%isNum% (fun x => x) || %isBool% (fun x => x) || %isFun% (fun x => x))",
            "false"
        );

        // Test laziness
        assert_peq!(
            "let throw = Assume(#fail, 0) in true || false || throw",
            "true"
        );
        assert_peq!(
            "let throw = Assume(#fail, 0) in true && false && throw",
            "false"
        );
        eval_string("let throw = Assume(#fail, 0) in false || true && throw").unwrap_err();
        eval_string("0 && true").unwrap_err();
        eval_string("\"a\" || false").unwrap_err();
    }

    #[test]
    fn records_contracts_simple() {
        assert_peq!("Assume({}, {})", "{}");
        eval_string("Assume({}, {a=1})").unwrap_err();

        assert_peq!(
            "let x = Assume({a: Num, s: Str}, {a = 1; s = \"a\"}) in %deepSeq% x x",
            "{a = 1; s = \"a\"}"
        );
        eval_string("let x = Assume({a: Num, s: Str}, {a = 1; s = 2}) in %deepSeq% x x")
            .unwrap_err();
        eval_string("let x = Assume({a: Num, s: Str}, {a = \"a\"; s = \"b\"}) in %deepSeq% x x")
            .unwrap_err();
        eval_string("let x = Assume({a: Num, s: Str}, {a = 1}) in %deepSeq% x x").unwrap_err();
        eval_string("let x = Assume({a: Num, s: Str}, {s = \"a\"}) in %deepSeq% x x").unwrap_err();
        eval_string(
            "let x = Assume({a: Num, s: Str}, {a = 1; s = \"a\"; extra = 1}) in %deepSeq% x x",
        )
        .unwrap_err();

        assert_peq!(
            "let x = Assume({a: Num, s: {foo: Bool}}, {a = 1; s = { foo = true}}) in %deepSeq% x x",
            "{a = 1; s = { foo = true}}"
        );
        eval_string(
            "let x = Assume({a: Num, s: {foo: Bool} }, {a = 1; s = { foo = 2}}) in %deepSeq% x x",
        )
        .unwrap_err();
        eval_string("let x = Assume({a: Num, s: {foo: Bool} }, {a = 1; s = { foo = true; extra = 1}}) in %deepSeq% x x").unwrap_err();
        eval_string("let x = Assume({a: Num, s: {foo: Bool} }, {a = 1; s = { }}) in %deepSeq% x x")
            .unwrap_err();
    }

    #[test]
    fn records_contracts_poly() {
        let id = "let f = Assume(forall a. { | a} -> { | a }, fun x => x) in f";
        let extd =
            "let f = Assume(forall a. { | a} -> {foo: Num | a}, fun x => x$[\"foo\"=1]) in f";
        let remv = "let f = Assume(forall a. {foo: Num | a} -> { | a}, fun x => x-$\"foo\") in f";

        assert_peq!(format!("{} {{}}", id), "{}");
        assert_peq!(format!("{} {{a=1; b=false}}", id), "{a=1; b=false}");

        assert_peq!(format!("{} {{}}", extd), "{foo = 1}");
        assert_peq!(
            format!("{} {{bar = false}}", extd),
            "{foo = 1; bar = false}"
        );
        // TODO: this test should ultimately pass (i.e., the program should be rejected)
        // but this is not yet the case.
        // eval_string(&format!("{} {{foo = 2}}", extd)).unwrap_err();

        assert_peq!(format!("{} {{foo = 1}}", remv), "{}");
        assert_peq!(format!("{} {{foo = 1; bar = 1}}", remv), "{bar = 1}");
        eval_string(&format!("{} {{}}", remv)).unwrap_err();

        assert_peq!(format!("{} ({} {{}})", remv, extd), "{}");
        assert_peq!(format!("{} ({} {{foo = 2}})", extd, remv), "{foo = 1}");

        assert_peq!(
            "
            let f = Assume(
                forall a .(forall b. {f: a -> a, arg: a | b} -> a),
                fun rec => rec.f (rec.arg)) in
            f { f = fun x => x ++ \" suffix\"; arg = \"blouh\" }",
            "\"blouh suffix\""
        );

        let bad_cst = "let f = Assume(forall a. { | a}} -> { | a}, fun x => {a=1}) in f";
        let bad_acc = "let f = Assume(forall a. { | a}} -> { | a}, fun x => seq (x.a) x) in f";
        let bad_extd =
            "let f = Assume(forall a. { | a} -> {foo: Num | a}, fun x => x-$\"foo\" in f";
        let bad_rmv =
            "let f = Assume(forall a. {foo: Num | a} -> { | a}, fun x => x$[\"foo\"=1]) in f";

        eval_string(&format!("{} {{}}", bad_cst)).unwrap_err();
        eval_string(&format!("{} {{a=1}}", bad_acc)).unwrap_err();
        eval_string(&format!("{} {{foo=1}}", bad_extd)).unwrap_err();
        eval_string(&format!("{} {{}}", bad_rmv)).unwrap_err();
        eval_string(
            "
            let f = Assume(
                forall a. ((forall b. ({a: Num, b: Num |b} })
                    -> ({ a: Num | b}))
                    -> {a: Num | a}
                    -> { | a}),
                fun f rec => (f rec) -$ \"a\" -$ \"b\") in
            f (fun x => x) {a = 1; b = bool; c = 3}",
        )
        .unwrap_err();
    }

    #[test]
    fn records_contracts_dyn() {
        assert_peq!(
            "Assume({a: Num, b: Str | Dyn}, {a = 1; b = \"b\"})",
            "{a = 1; b = \"b\"}"
        );
        assert_peq!(
            "Assume({a: Num, b: Str | Dyn}, {a = 1; b = \"b\"; c = false})",
            "{a = 1; b = \"b\"; c = false}"
        );
        assert_peq!(
            "Assume({a: Num | Dyn} -> Dyn, fun r => r.b) {a = 1; b = 2}",
            "2"
        );
        eval_string("Assume({a: Num, b: Str | Dyn}, {a = 1})").unwrap_err();
    }

    #[test]
    fn lists_contracts() {
        use crate::label::ty_path::Elem;

        assert_peq!("[1, \"2\", false] | List", "[1, \"2\", false]");
        assert_peq!("[1, 2, 3] | List Num", "[1, 2, 3]");
        assert_peq!(
            "[\"1\", \"2\", \"false\"] | List Str",
            "[\"1\", \"2\", \"false\"]"
        );

        assert_matches!(
            eval_string("%deepSeq% ([1, \"a\"] | List Num) 0"),
            Err(Error::EvalError(EvalError::BlameError(..)))
        );
        assert_matches!(
            eval_string("1 | List"),
            Err(Error::EvalError(EvalError::BlameError(..)))
        );
        assert_matches!(
            eval_string("(fun x => x) | List"),
            Err(Error::EvalError(EvalError::BlameError(..)))
        );

        let res = eval_string("%deepSeq% ([{a = [1]}] | List {a: List Str}) false");
        match &res {
            Err(Error::EvalError(EvalError::BlameError(ref l, _))) => {
                assert_matches!(l.path.as_slice(), [Elem::List, Elem::Field(id), Elem::List] if &id.to_string() == "a")
            }
            err => panic!("expected blame error, got {:?}", err),
        }
        // Check that reporting doesn't panic. Provide a dummy file database, as we won't report
        // the error message but just check that it can be built.
        let mut files = Files::new();
        res.unwrap_err().to_diagnostic(&mut files, None);

        let res = eval_string("(%elemAt% (({foo = [(fun x => \"a\")]} | {foo: List (forall a. a -> Num)}).foo) 0) false");
        match &res {
            Err(Error::EvalError(EvalError::BlameError(ref l, _))) => {
                assert_matches!(l.path.as_slice(), [Elem::Field(id), Elem::List, Elem::Codomain] if &id.to_string() == "foo")
            }
            err => panic!("expected blame error, got {:?}", err),
        }
        res.unwrap_err().to_diagnostic(&mut files, None);
    }

    #[test]
    fn multiline_string_indent() {
        // /!\ Trailing spaces on the first line are on purpose, don't remove ! /!\
        assert_peq!(
            r##"  
                m#"
                   this
                       is an
                       indented
                   text
                "#m
            "##,
            "\"this\n    is an\n    indented\ntext\""
        );

        // /!\ Trailing spaces on the first line are on purpose, don't remove ! /!\
        assert_peq!(
            r##"  
                m#"
                   this
                       is an
                       indented
                   text"#m
            "##,
            "\"this\n    is an\n    indented\ntext\""
        );

        // /!\ Trailing spaces on the middle line are on purpose, don't remove ! /!\
        assert_peq!(
            r##"
                m#"
                   ignore
    
                    empty line indent
                "#m
            "##,
            "\"ignore\n\n empty line indent\""
        );

        // /!\ Trailing spaces on the middle line are on purpose, don't remove ! /!\
        assert_peq!(
            r##"
                m#"
    
                   ignore
                    first line indent
                "#m
            "##,
            "\"\nignore\n first line indent\""
        );
    }

    #[test]
    fn multiline_string_delimiters() {
        // Writing multi-line strings inside Rust raw strings, which have the similar delimiters
        // is, is not really pretty
        assert_eq!(
            eval_string(r###"m##""#m"##a"##m"###),
            Ok(Term::Str(String::from(r###""#m"##a"###))),
        );

        assert_eq!(
            eval_string(r####"m###""##m"###a"###m"####),
            Ok(Term::Str(String::from(r####""##m"###a"####))),
        );
    }

    #[test]
    fn multiline_interpolation() {
        assert_peq!(
            r##"m#"Simple #{"interpolated"} string"#m"##,
            "\"Simple interpolated string\""
        );
        assert_peq!(
            r###"m##"Double ##{"interpolated"} string"##m"###,
            "\"Double interpolated string\""
        );
        assert_peq!(
            r###"m##"Not #{"interpolated"}"##m"###,
            "\"Not \\#{\\\"interpolated\\\"}\""
        );
        assert_peq!(
            r####"m###"###{"Triple"} ##{not} #{interpolated}"###m"####,
            "\"Triple #\\#{not} \\#{interpolated}\""
        );
        assert_peq!(
            r###"m#"#{m##"##{"Not"} #{interpolated}"##m} ##{"string"}"#m"###,
            "\"Not \\#{interpolated} #\\#{\\\"string\\\"}\""
        );

        assert_peq!(
            r###"
                m#"
                   #{m#"thi"#m ++ "s"}
                       #{"is" ++ " an"}
                       indented
                   #{"#{m##"te"##m}xt"}
                "#m
            "###,
            "\"this\n    is an\n    indented\ntext\""
        );

        assert_peq!(
            r##"
                let x = "I\n need\n  indent!" in
                m#"
                  base
                    #{x}
                  #{x}
                "#m
            "##,
            r#""base
  I
   need
    indent!
I
 need
  indent!""#
        );

        assert_peq!(
            r##"
                let x = "ignore\nmy\nindent" in
                let y = "me\ntoo" in
                m#"
                  strip
                    #{x} #{y}
                    #{"not\nme"}
                "#m
            "##,
            r#""strip
  ignore
my
indent me
too
  not
  me""#
        );
    }

    #[test]
    fn evaluation_full() {
        use crate::mk_record;
        use crate::term::make as mk_term;
        use std::mem;

        // Clean all the position information in a term.
        fn clean_pos(t: Term) -> Term {
            let mut tmp = RichTerm::new(t, None);
            tmp.clean_pos();
            *tmp.term
        }

        let t =
            clean_pos(eval_string_full("[(1 + 1), (\"a\" ++ \"b\"), ([ 1, [1 + 2] ])]").unwrap());
        let mut expd = parse("[2, \"ab\", [1, [3]]]").unwrap();
        // String are parsed as StrChunks, but evaluated to Str, so we need to hack list a bit
        if let Term::List(ref mut data) = *expd.term {
            mem::replace(data.get_mut(1).unwrap(), mk_term::string("ab"));
        } else {
            panic!();
        }
        assert_eq!(t, *expd.term);

        let t = clean_pos(
            eval_string_full(
                "let x = 1 in let y = 1 + x in let z = { foo = {bar = { baz  = y } } } in z",
            )
            .unwrap(),
        );
        // Records are parsed as RecRecords, so we need to build one by hand
        let expd = mk_record!((
            "foo",
            mk_record!(("bar", mk_record!(("baz", Term::Num(2.0)))))
        ));
        assert_eq!(t, *expd.term);

        // /!\ [MAY OVERFLOW STACK]
        // Check that substitution do not replace bound variables. Before the fixing commit, this
        // example would go into an infinite loop, and stack overflow. If it does, this just means
        // that this test fails.
        eval_string_full("{y = fun x => x; x = fun y => y}").unwrap();
    }
}
