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
//! ```ignore
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
        eval::env_add(
            &mut env,
            Ident::from("y"),
            cache.get_owned(file_id).unwrap(),
            eval::Environment::new(),
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
    fn enum_simple() {
        eval_string("`far : <foo, bar>").unwrap_err();
    }

    #[test]
    fn enum_complex() {
        eval_string(
            "let f : <foo, bar> -> Num =
            fun x => switch { foo => 1, bar => 2, } x in
            f `boo",
        )
        .unwrap_err();
    }

    #[test]
    fn row_types() {
        eval_string("Assume(< >, 123)").unwrap_err();
    }

    #[test]
    fn records_accessing() {
        eval_string("({ $(if false then \"foo\" else \"bar\") = false; bar = true; }).foo")
            .unwrap_err();
    }

    #[test]
    fn lists() {
        eval_string("%elemAt% [1,2,3] (-1)").unwrap_err();
        eval_string("%elemAt% [1,2,3] 4").unwrap_err();

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
    }

    #[test]
    #[should_panic]
    fn merge_record_failure() {
        eval_string("({a=1} & {a=2}).a").unwrap();
    }

    #[test]
    #[should_panic]
    fn enriched_terms_contract_default_fail() {
        eval_string("ContractDefault(Num, true)").unwrap();
    }

    #[test]
    fn merge_default() {
        eval_string("({a=Default(1);} & {a=Default(2);}).a").unwrap_err();
    }

    #[test]
    fn merge_contract() {
        eval_string("let r = {a=2;} & {a=Contract(Bool)} in r.a").unwrap_err();
    }

    #[test]
    fn merge_default_contract() {
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
        make_composed_contract("1").unwrap_err();
        make_composed_contract("14").unwrap_err();
        make_composed_contract("27").unwrap_err();
        make_composed_contract("10").unwrap_err();
        make_composed_contract("35").unwrap_err();
    }

    #[test]
    fn string_chunks() {
        match eval_string(r##""bad type #{1 + 1}""##) {
            Err(Error::EvalError(EvalError::TypeError(_, _, _, _))) => (),
            _ => assert!(false),
        };
    }

    #[test]
    fn arithmetic_expr() {
        eval_string("1 + 1 / (1 - 1)").unwrap_err();
    }

    #[test]
    fn comparisons() {
        eval_string("1 < 2 < 3").unwrap_err();
        eval_string("1 < 2 > 3").unwrap_err();
        eval_string("\"a\" < 2").unwrap_err();
        eval_string("true <= []").unwrap_err();
        eval_string("\"a\" > \"b\"").unwrap_err();
        eval_string("\"a\" >= \"b\"").unwrap_err();
    }

    #[test]
    fn boolean_op() {
        eval_string("let throw = Assume(#fail, 0) in false || true && throw").unwrap_err();
        eval_string("0 && true").unwrap_err();
        eval_string("\"a\" || false").unwrap_err();
    }

    #[test]
    fn records_contracts_simple() {
        eval_string("Assume({}, {a=1})").unwrap_err();

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
        // TODO: this test should ultimately pass (i.e., the program should be rejected)
        // but this is not yet the case.
        // eval_string(&format!("{} {{foo = 2}}", extd)).unwrap_err();

        let remv = "let f = Assume(forall a. {foo: Num | a} -> { | a}, fun x => x-$\"foo\") in f";
        eval_string(&format!("{} {{}}", remv)).unwrap_err();

        let bad_cst = "let f = Assume(forall a. { | a} -> { | a}, fun x => {a=1}) in f";
        let bad_acc = "let f = Assume(forall a. { | a} -> { | a}, fun x => seq (x.a) x) in f";
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
    fn lists_contracts() {
        use crate::label::ty_path::Elem;

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
    fn evaluation_full() {
        use crate::mk_record;
        use crate::term::make as mk_term;

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
            *data.get_mut(1).unwrap() = mk_term::string("ab");
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

    #[test]
    fn infinite_loops() {
        assert_matches!(
            eval_string("{x = x}.x"),
            Err(Error::EvalError(EvalError::InfiniteRecursion(..)))
        );
        assert_matches!(
            eval_string("{x = y; y = z; z = x }.x"),
            Err(Error::EvalError(EvalError::InfiniteRecursion(..)))
        );
        assert_matches!(
            eval_string("{x = y + z; y = z + x; z = 1}.x"),
            Err(Error::EvalError(EvalError::InfiniteRecursion(..)))
        );
        assert_matches!(
            eval_string("{x = (fun a => a + y) 0; y = (fun a => a + x) 0}.x"),
            Err(Error::EvalError(EvalError::InfiniteRecursion(..)))
        );
    }
}
