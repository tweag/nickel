//! Program handling, from file reading to evaluation.
//!
//! A program is Nickel source code loaded from an input. This module offers an interface to load a
//! program source, parse it, evaluate it and report errors.
//!
//! # Builtin contracts
//!
//! Builtins contracts are the essential contracts, written in pure Nickel, which are required by
//! the Nickel abstract machine. They are automatically inserted when evaluating terms like
//! `Assume(type, term)`. They are currently just being pasted at the beginning of the input, which
//! is bad, but this will be corrected once we have settled on and implemented imports.
use crate::error::{Error, ImportError, ParseError, ToDiagnostic};
use crate::eval;
use crate::parser;
use crate::position::RawSpan;
use crate::term::{RichTerm, Term};
use crate::transformations;
use crate::typecheck::type_check;
use codespan::{FileId, Files};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::collections::HashMap;
use std::ffi::OsString;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::result::Result;

/// A Nickel program.
///
/// Manage a file database, which stores the original source code of the program and eventually the
/// code of imported expressions, and a dictionary which stores corresponding parsed terms.
pub struct Program {
    /// Control if the built-in contracts are included (pasted at the beginning of the source).
    include_contracts: bool,
    /// The id of the program source in the file database.
    main_id: FileId,
    /// The file database holding the content of the program source plus potential imports
    /// made by the program (imports will be supported in a near future).
    files: Files<String>,
    /// Cache storing the id of files that are already stored in the database.
    file_cache: HashMap<String, FileId>,
    /// Cache storing parsed terms corresponding to the entries of the file database.
    term_cache: HashMap<FileId, RichTerm>,
}

/// Return status indicating if an import has been resolved from a file (first encounter), or was
/// retrieved from the cache.
///
/// See [`resolve`](./fn.resolve.html).
#[derive(Debug, PartialEq)]
pub enum ResolvedTerm {
    FromFile(
        /* the parsed term */ RichTerm,
        /* the path of the loaded file */ PathBuf,
    ),
    FromCache(),
}

/// Abstract the access to imported files and the import cache. Used by the evaluator, the
/// typechecker and at [import resolution](../transformations/import_resolution/index.html) phase.
///
/// The standard implementation use 2 caches, the file cache for raw contents and the term cache
/// for parsed contents, mirroring the 2 steps when resolving an import:
/// 1. When an import is encountered for the first time, the content of the corresponding file is
///    read and stored in the file cache (consisting of the file database plus a map between paths
///    and ids in the database). The content is parsed, and this term is queued somewhere so that
///    it can undergo the standard [transformations](../transformations/index.html) first, but is
///    not stored in the term cache yet.
/// 2. When it is finally processed, the term cache is updated with the transformed term.
pub trait ImportResolver {
    /// Resolve an import.
    ///
    /// Read and store the content of an import, put it in the file cache (or get it from there if
    /// it is cached), then parse it and return the corresponding term and file id.
    ///
    /// The term and the path are provided only if the import is processed for the first time.
    /// Indeed, at import resolution phase, the term of an import encountered for the first time is
    /// queued to be processed (e.g. having its own imports resolved). The path is needed to
    /// resolve nested imports relatively to this parent. Only after this processing the term is
    /// inserted back in the cache via [`insert`](#tymethod.insert). On the other hand, if it has
    /// been resolved before, it is already transformed in the cache and do not need further
    /// processing.
    fn resolve(
        &mut self,
        path: &String,
        parent: Option<PathBuf>,
        pos: &Option<RawSpan>,
    ) -> Result<(ResolvedTerm, FileId), ImportError>;

    /// Insert an entry in the term cache after transformation.
    fn insert(&mut self, file_id: FileId, term: RichTerm);

    /// Get a resolved import from the term cache.
    fn get(&self, file_id: FileId) -> Option<RichTerm>;

    /// Get a file id from the file cache.
    fn get_id(&self, path: &String, parent: Option<PathBuf>) -> Option<FileId>;
}

impl Program {
    /// Create a program by reading it from the standard input.
    pub fn new_from_stdin() -> std::io::Result<Program> {
        Program::new_from_source(io::stdin(), "<stdin>")
    }

    /// Create a program by reading from a file.
    pub fn new_from_file<P: AsRef<Path>>(path: P) -> std::io::Result<Program> {
        let file = fs::File::open(&path)?;
        Program::new_from_source(file, path.as_ref())
    }

    /// Create a program by reading it from a generic source.
    fn new_from_source<T: Read>(
        mut source: T,
        source_name: impl Into<OsString>,
    ) -> std::io::Result<Program> {
        let mut buffer = String::new();
        let mut files = Files::<String>::new();

        source.read_to_string(&mut buffer)?;
        let main_id = files.add(source_name, buffer);

        Ok(Program {
            include_contracts: true,
            main_id,
            files,
            file_cache: HashMap::new(),
            term_cache: HashMap::new(),
        })
    }

    /// Parse if necessary, typecheck and then evaluate the program.
    pub fn eval(&mut self) -> Result<Term, Error> {
        let t = self
            .parse_with_cache(self.main_id)
            .map_err(|e| Error::from(e))?;
        let t = transformations::transform(t, self).map_err(|err| Error::ImportError(err))?;
        println!("Typechecked: {:?}", type_check(t.as_ref(), self));
        eval::eval(t, self).map_err(|e| e.into())
    }

    /// Parse a source file. Do not try to get it from the cache, and do not populate the cache at
    /// the end either.
    fn parse(&mut self, file_id: FileId) -> Result<RichTerm, ParseError> {
        let mut buf = self.files.source(file_id).clone();
        if self.include_contracts {
            // TODO get rid of this once we have imports
            buf.insert_str(0, &Self::contracts());
        }

        let offset = if self.include_contracts {
            Self::contracts().len()
        } else {
            0
        };
        parser::grammar::TermParser::new()
            .parse(&file_id, offset, &buf)
            .map_err(|err| ParseError::from_lalrpop(err, file_id, offset))
    }

    /// Parse a source file and populate the corresponding entry in the cache, or just get it from
    /// the term cache if it is there. Return a copy of the cached term.
    fn parse_with_cache(&mut self, file_id: FileId) -> Result<RichTerm, ParseError> {
        Ok(match self.term_cache.get(&file_id) {
            Some(t) => t.clone(),
            None => {
                let t = self.parse(file_id)?;
                self.term_cache.insert(file_id, t.clone());
                t
            }
        })
    }

    /// Pretty-print an error.
    ///
    /// This function is located here in `Program` because errors need a reference to `files` in
    /// order to produce a diagnostic (see [`label_alt`](../error/fn.label_alt.html)).
    pub fn report(&mut self, error: &Error) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        let diagnostic = error.to_diagnostic(&mut self.files);
        match codespan_reporting::term::emit(&mut writer.lock(), &config, &self.files, &diagnostic)
        {
            Ok(()) => (),
            Err(err) => panic!(
                "Program::report: could not print an error on stderr: {}",
                err
            ),
        };
    }

    /// Built-in contracts to be included in programs.
    // TODO: move this to a Nickel stand-alone file once we have imports
    fn contracts() -> String {
        "let dyn = fun l t => t in

        let num = fun l t => if isNum t then t else blame (tag[num] l) in

        let bool = fun l t => if isBool t then t else blame (tag[bool] l) in

        let string = fun l t => if isStr t then t else blame (tag[str] l) in

        let list = fun l t => if isList t then t else blame (tag[list] l) in

        let func = fun s t l e =>
            let l = tag[func] l in
            if isFun e then (fun x => t (goCodom l) (e (s (chngPol (goDom l)) x)))
            else blame l in

        let forall_var = fun sy pol l t =>
            let lPol = polarity l in
            if pol =b lPol then unwrap sy t (blame (tag[unwrp] l))
            else wrap sy t in

        let fail = fun l t => blame (tag[fail] l) in

        let row_extend = fun contr case l t =>
            if (case t) then t else contr (tag[NotRowExt] l) t in
        "
        .to_string()
    }
}

impl ImportResolver for Program {
    fn resolve(
        &mut self,
        path: &String,
        parent: Option<PathBuf>,
        pos: &Option<RawSpan>,
    ) -> Result<(ResolvedTerm, FileId), ImportError> {
        let (path_buf, normalized) = with_parent(path, parent);

        if let Some(file_id) = self.file_cache.get(&normalized) {
            return Ok((ResolvedTerm::FromCache(), *file_id));
        }

        let mut buffer = String::new();
        let file_id = fs::File::open(path_buf)
            .and_then(|mut file| file.read_to_string(&mut buffer))
            .map(|_| self.files.add(path, buffer))
            .map_err(|err| ImportError::IOError(path.clone(), format!("{}", err), pos.clone()))?;
        self.file_cache.insert(normalized, file_id.clone());

        let t = self
            .parse(file_id)
            .map_err(|err| ImportError::ParseError(err, pos.clone()))?;
        Ok((
            ResolvedTerm::FromFile(t, Path::new(path).to_path_buf()),
            file_id,
        ))
    }

    fn get(&self, file_id: FileId) -> Option<RichTerm> {
        self.term_cache.get(&file_id).cloned()
    }

    fn get_id(&self, path: &String, parent: Option<PathBuf>) -> Option<FileId> {
        let (_, normalized) = with_parent(path, parent);
        self.file_cache.get(&normalized).cloned()
    }

    fn insert(&mut self, file_id: FileId, term: RichTerm) {
        self.term_cache.insert(file_id, term);
    }
}

/// Compute the path of a file relatively to a parent, and a string representation of the
/// normalized full path (see [`normalize_path`](./fn.normalize_path.html). If the path is absolute
/// or if the parent is `None`, the first component is the same as `Path::new(path).to_path_buf()`.
fn with_parent(path: &String, parent: Option<PathBuf>) -> (PathBuf, String) {
    let mut path_buf = parent.unwrap_or(PathBuf::new());
    path_buf.pop();
    path_buf.push(Path::new(path));
    let normalized = normalize_path(path_buf.as_path()).unwrap_or_else(|| path.clone());

    (path_buf, normalized)
}

/// Normalize the path of a file to uniquely identify names in the cache.
///
/// If an IO error occurs here, `None` is returned.
fn normalize_path(path: &Path) -> Option<String> {
    path.canonicalize()
        .ok()
        .map(|p_| p_.to_string_lossy().into_owned())
}

/// Provide mockup import resolvers for testing purpose.
#[cfg(test)]
pub mod resolvers {
    use super::*;

    /// A dummy resolver that panics when asked to do something. Used to test code that contains no
    /// import.
    pub struct DummyResolver {}

    impl ImportResolver for DummyResolver {
        fn resolve(
            &mut self,
            _path: &String,
            _parent: Option<PathBuf>,
            _pos: &Option<RawSpan>,
        ) -> Result<(ResolvedTerm, FileId), ImportError> {
            panic!("program::resolvers: dummy resolver should not have been invoked");
        }

        fn insert(&mut self, _file_id: FileId, _term: RichTerm) {
            panic!("program::resolvers: dummy resolver should not have been invoked");
        }

        fn get(&self, _file_id: FileId) -> Option<RichTerm> {
            panic!("program::resolvers: dummy resolver should not have been invoked");
        }

        fn get_id(&self, _path: &String, _parent: Option<PathBuf>) -> Option<FileId> {
            panic!("program::resolvers: dummy resolver should not have been invoked");
        }
    }

    /// Resolve imports from a mockup file database. Used to test imports without accessing the
    /// file system.
    pub struct SimpleResolver {
        files: Files<String>,
        file_cache: HashMap<String, FileId>,
        term_cache: HashMap<FileId, Option<RichTerm>>,
    }

    impl SimpleResolver {
        pub fn new() -> SimpleResolver {
            SimpleResolver {
                files: Files::new(),
                file_cache: HashMap::new(),
                term_cache: HashMap::new(),
            }
        }

        /// Add a mockup file to available imports.
        pub fn add_source(&mut self, name: String, source: String) {
            let id = self.files.add(&name, source);
            self.file_cache.insert(name, id);
        }
    }

    impl ImportResolver for SimpleResolver {
        fn resolve(
            &mut self,
            path: &String,
            _parent: Option<PathBuf>,
            pos: &Option<RawSpan>,
        ) -> Result<(ResolvedTerm, FileId), ImportError> {
            let file_id =
                self.file_cache
                    .get(path)
                    .map(|id| id.clone())
                    .ok_or(ImportError::IOError(
                        path.clone(),
                        String::from("Import not found by the mockup resolver."),
                        pos.clone(),
                    ))?;

            if self.term_cache.contains_key(&file_id) {
                Ok((ResolvedTerm::FromCache(), file_id))
            } else {
                self.term_cache.insert(file_id, None);
                let buf = self.files.source(file_id);
                let t = parser::grammar::TermParser::new()
                    .parse(&file_id, 0, &buf)
                    .map_err(|e| ParseError::from_lalrpop(e, file_id, 0))
                    .map_err(|e| ImportError::ParseError(e, pos.clone()))?;
                Ok((ResolvedTerm::FromFile(t, PathBuf::new()), file_id))
            }
        }

        fn insert(&mut self, file_id: FileId, term: RichTerm) {
            self.term_cache.insert(file_id, Some(term));
        }

        fn get(&self, file_id: FileId) -> Option<RichTerm> {
            self.term_cache
                .get(&file_id)
                .map(|opt| opt.as_ref())
                .flatten()
                .cloned()
        }

        fn get_id(&self, path: &String, _parent: Option<PathBuf>) -> Option<FileId> {
            self.file_cache.get(path).copied()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::EvalError;
    use crate::identifier::Ident;
    use std::io::Cursor;

    fn eval_string(s: &str) -> Result<Term, Error> {
        let src = Cursor::new(s);

        let mut p = Program::new_from_source(src, "<test>").map_err(|io_err| {
            Error::EvalError(EvalError::Other(format!("IO error: {}", io_err), None))
        })?;
        p.eval()
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
                m.remove(&Ident(String::from(i)))
                    .unwrap_or_else(|| panic!(format!("Could not find field {} in result", i)));

                let proj = format!("({}).{}", s, i);
                if let Ok(proj_res) = eval_string(&proj) {
                    assert_eq!(proj_res, t);
                } else {
                    panic!(format!("evaluation of the projection on {} failed", i));
                }
            }

            assert!(m.is_empty());
        } else {
            panic!("evaluation failed");
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
        let res = eval_string("let x = 5 in if isNum x then true else 1");

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
            "let safePlus = Promise(Num -> Num -> Num , fun x => fun y => x + y) in
safePlus Promise(Num , 54) Promise(Num , 6)",
        );

        assert_eq!(Ok(Term::Num(60.)), res);
    }

    #[test]
    fn fibonacci() {
        let res = eval_string(
            "let Y = Assume(((Num -> Num) -> Num -> Num) -> Num -> Num, fun f => (fun x => f (x x)) (fun x => f (x x))) in
let dec = Promise(Num -> Num, fun x => x + (-1)) in
let or = Promise(Bool -> Bool -> Bool, fun x => fun y => if x then x else y) in

let fibo = Promise(Num -> Num, Y (fun fibo =>
    (fun x => if or (isZero x) (isZero (dec x)) then 1 else (fibo (dec x)) + (fibo (dec (dec x)))))) in
let val = Promise(Num, 4) in
fibo val",
        );

        assert_eq!(Ok(Term::Num(5.)), res);
    }

    #[test]
    fn promise_fail() {
        let res = eval_string(
            "let bool = fun l t => if isBool t then t else blame l in

Promise(Bool, 5)
            ",
        );

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
            "let alwaysTrue = fun l t => let boolT = Assume(Bool, t) in
    if boolT then boolT else blame l in
let alwaysFalse = fun l t => let boolT = Assume(Bool, t) in
    if boolT then  blame l else boolT in
let not = fun b => if b then false else true in
Assume(#alwaysTrue -> #alwaysFalse, not ) true
",
        );

        assert_eq!(Ok(Term::Bool(false)), res);
    }

    #[test]
    fn safe_id() {
        let res = eval_string(
            "let id = Assume(forall a . a -> a, fun x => x) in
            id false",
        );
        assert_eq!(Ok(Term::Bool(false)), res);
    }

    #[test]
    fn id_fail() {
        let res = eval_string(
            "let id = Assume(forall a . a -> a, fun x => false) in
            id false",
        );
        if let Ok(_) = res {
            panic!("This expression should return an error!");
        }
    }

    #[test]
    fn safe_higher_order() {
        let res = eval_string(
            "let to_bool = Assume(forall a . (a -> Bool) -> a -> Bool,
            fun f => fun x => f x) in
            to_bool (fun x => true) 4 ",
        );
        assert_eq!(Ok(Term::Bool(true)), res);
    }

    #[test]
    fn apply_twice() {
        let res = eval_string(
            "let twice = Assume(forall a . (a -> a) -> a -> a,
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
        let res = eval_string("Promise(< (| foo, bar, |) >, `foo)");
        assert_eq!(res, Ok(Term::Enum(Ident("foo".to_string()))));

        let res = eval_string("Promise(forall r. (< (| foo, bar, | r ) >), `bar)");
        assert_eq!(res, Ok(Term::Enum(Ident("bar".to_string()))));

        eval_string("Promise(< (| foo, bar, |) >, `far)").unwrap_err();
    }

    #[test]
    fn enum_complex() {
        let res = eval_string(
            "let f = Promise(forall r. < (| foo, bar, | r ) > -> Num,
        fun x => switch { foo => 1, bar => 2, _ => 3, } x) in
        f `bar",
        );
        assert_eq!(res, Ok(Term::Num(2.)));

        let res = eval_string(
            "let f = Promise(forall r. < (| foo, bar, | r ) > -> Num,
        fun x => switch { foo => 1, bar => 2, _ => 3, } x) in
        f `boo",
        );
        assert_eq!(res, Ok(Term::Num(3.)));

        eval_string(
            "let f = Promise(< (| foo, bar, |) > -> Num,
        fun x => switch { foo => 1, bar => 2, } x) in
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
        eval_string("Assume((| |), 123)").unwrap_err();
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
            eval_string("hasField \"foo\" { foo = 1; bar = 2; }"),
            Ok(Term::Bool(true))
        );
        assert_eq!(
            eval_string("hasField \"fop\" { foo = 1; bar = 2; }"),
            Ok(Term::Bool(false))
        );

        assert_eq!(
            eval_string("(mapRec (fun y => fun x => x + 1) { foo = 1; bar = \"it's lazy\"; }).foo"),
            Ok(Term::Num(2.)),
        );
        assert_eq!(
            eval_string(
                "let r = mapRec
                    (fun y x => if isNum x then x + 1 else 0)
                    { foo = 1; bar = \"it's lazy\"; }
                in
                (r.foo) + (r.bar)"
            ),
            Ok(Term::Num(2.)),
        );

        assert_eq!(
            eval_string("hasField \"foo\" ( { foo = 2; bar = 3; }-$(\"foo\"))"),
            Ok(Term::Bool(false))
        );

        assert_eq!(
            eval_string("hasField \"foo\" ( { bar = 3; }$[\"foo\" = 1])"),
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
        assert_eq!(eval_string("seq 1 true"), Ok(Term::Bool(true)));
        assert_eq!(
            eval_string("let x = (1 + 1) in seq x x"),
            Ok(Term::Num(2.0))
        );

        assert_eq!(
            eval_string("let r = {a=(1 + 1);} in deepSeq r (r.a)"),
            Ok(Term::Num(2.0))
        );
        assert_eq!(
            eval_string("let r = {a=(1 + 1);b=(\"a\" ++ \"b\");} in deepSeq r (r.b)"),
            Ok(Term::Str(String::from("ab")))
        );
        assert_eq!(
            eval_string("let r = {a={b=(1 + 1);};} in deepSeq r ((r.a).b)"),
            Ok(Term::Num(2.0))
        );

        assert_eq!(
            eval_string(
                "let inj = fun x => {b=(x + 2);} in
                let cat = fun x => fun y => x ++ y in
                let r = {a=(inj 1);b=(cat \"a\" \"b\");} in deepSeq r ((r.a).b)"
            ),
            Ok(Term::Num(3.0))
        )
    }

    #[test]
    fn lists() {
        assert_eq!(eval_string("elemAt [1,2,3] 1"), Ok(Term::Num(2.0)));
        assert_eq!(
            eval_string("elemAt (map (fun x => x + 1) [1,2,3]) 1"),
            Ok(Term::Num(3.0))
        );

        eval_string("elemAt [1,2,3] (-1)").unwrap_err();
        eval_string("elemAt [1,2,3] 4").unwrap_err();

        assert_eq!(eval_string("length []"), Ok(Term::Num(0.0)));
        assert_eq!(eval_string("length [1,2,3]"), Ok(Term::Num(3.0)));

        assert_eq!(
            eval_string("length ([] @ [1,2] @ [3,4] @ [])"),
            Ok(Term::Num(4.0))
        );

        assert_eq!(
            eval_string("head [\"a\",\"b\",\"c\"]"),
            Ok(Term::Str(String::from("a")))
        );
        eval_string("head []").unwrap_err();

        assert_eq!(
            eval_string("length (tail [true,false,1])"),
            Ok(Term::Num(2.0))
        );
        eval_string("tail []").unwrap_err();

        assert_eq!(
            eval_string(
                "let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
                let foldr_ =
                    fun self => fun f => fun acc => fun l =>
                        if isZero (length l) then acc
                        else
                            let h = head l in
                            let t = tail l in
                            let next_acc = self f acc t in
                            f next_acc h
                in
                let foldr = Y foldr_ in
                let and = Promise(Bool -> Bool -> Bool,
                    fun x => fun y =>
                        if x then
                            if y then true else false
                        else false)
                in
                let all = fun pred => fun l => foldr and true (map pred l) in
                let isZ = fun x => isZero x in
                all isZ [0, 0, 0, 1]"
            ),
            Ok(Term::Bool(false))
        );
    }

    #[test]
    fn merge_record_simple() {
        assert_eval_to_record(
            "merge {a=1;} {b=true;}",
            vec![("a", Term::Num(1.0)), ("b", Term::Bool(true))],
        );
    }

    #[test]
    #[should_panic]
    fn merge_record_failure() {
        eval_string("(merge {a=1} {a=2}).a").unwrap();
    }

    #[test]
    fn merge_record_intersection() {
        assert_eval_to_record(
            "merge {a=1;b=2;} {b=2;c=3;}",
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
            "(merge {a={b=1;};} {a={c=true;};}).a ",
            vec![("b", Term::Num(1.0)), ("c", Term::Bool(true))],
        );
    }

    #[test]
    fn merge_record_complex() {
        assert_eval_to_record(
            "let rec1 = {a=false;b=(if true then (1 + 1) else (2 + 0)); c=(((fun x => x) (fun y => y)) 2);} in
             let rec2 = {b=(((fun x => x) (fun y => y)) 2); c=(if true then (1 + 1) else (2 + 0)); d=true;} in
             merge rec1 rec2",
             vec![("a", Term::Bool(false)), ("b", Term::Num(2.0)), ("c", Term::Num(2.0)), ("d", Term::Bool(true))]
         );
    }

    #[test]
    fn merge_record_with_env() {
        assert_eq!(
            eval_string("((fun y => merge ((fun x => {a=y;}) 1) ({b=false;})) 2).a"),
            Ok(Term::Num(2.0))
        );
    }

    #[test]
    fn merge_record_with_env_nested() {
        assert_eq!(
            eval_string(
                "let rec = merge ({b={c=10;};}) ((fun x => {a=x; b={c=x;};}) 10) in
                         (rec.b).c"
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

    #[test]
    fn merge_default() {
        assert_eval_to_record(
            "merge {a=2;} {a=Default(0);b=Default(true);}",
            vec![("a", Term::Num(2.0)), ("b", Term::Bool(true))],
        );

        assert_eval_to_record(
            "(merge {a=Default({x=1;});} {a=Default({y=\"y\";});}).a",
            vec![("x", Term::Num(1.0)), ("y", Term::Str(String::from("y")))],
        );

        eval_string("(merge {a=Default(1);} {a=Default(2);}).a").unwrap_err();
    }

    #[test]
    fn merge_contract() {
        assert_eval_to_record(
            "merge {a=2;b=Contract(Bool);} {a=Contract(Num);b=Default(true);}",
            vec![("a", Term::Num(2.0)), ("b", Term::Bool(true))],
        );

        eval_string("let r = merge {a=2;} {a=Contract(Bool)} in r.a").unwrap_err();
    }

    #[test]
    fn merge_default_contract() {
        assert_eval_to_record(
            "merge {a=2;} {a=ContractDefault(Num, 0);b=Default(true);}",
            vec![("a", Term::Num(2.0)), ("b", Term::Bool(true))],
        );

        assert_eval_to_record(
            "merge (merge {a=2;} {a=Contract(Num);}) {a=Default(3);}",
            vec![("a", Term::Num(2.0))],
        );

        assert_eq!(
            eval_string("(merge (merge {a=2;} {b=Contract(Num);}) {b=Default(3);}).b"),
            Ok(Term::Num(3.0)),
        );

        assert_eq!(
            eval_string("(merge (merge {a=Default(1);} {b=Contract(Num);}) {a=Default(1);}).a"),
            Ok(Term::Num(1.0)),
        );

        eval_string("(merge (merge {a=2;} {b=Contract(Num);}) {b=Default(true);}).b").unwrap_err();
    }

    fn make_composed_contract(value: &str) -> Result<Term, Error> {
        let s = format!(
            "let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
             let dec = fun x => x + (-1) in
             let or = fun x => fun y => if x then x else y in
             let isEven_ = Y (fun f =>
                 (fun x =>
                     if (isZero x) then true
                     else (
                         if (isZero (dec x)) then false
                         else (f (dec (dec x)))
                     )
                 )
             ) in
             let isDivBy3_ = Y (fun f =>
                 (fun x =>
                     if (isZero x) then true
                     else (
                         if or (isZero (dec (dec x))) (isZero (dec x)) then false
                         else (f (dec (dec (dec x))))
                     )
                 )
             ) in
             let toCtr = fun f => fun l => fun x => (
               if (isNum x) then (
                 if (f x) then x else blame l)
               else blame l
             ) in
             let isEven = toCtr isEven_ in
             let isDivBy3 = toCtr isDivBy3_ in
             let composed = merge {{a=Contract(#isEven);}} {{a=Contract(#isDivBy3);}} in
             (merge {{a={};}} composed).a",
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
                format!("merge ({}) ({})", op, term)
            });
            eval_string(&format!("{} ({}).a", definitions, term))
        };

        // contract/contract -> contract/value -> value/default
        assert_eq!(
            merge_elts(vec!["ctr_num", "ctr_id", "val", "def"]),
            Ok(Term::Num(1.0))
        );
        // default/value <- value/contract
        assert_eq!(
            merge_elts(vec!["def", "merge val ctr_num"]),
            Ok(Term::Num(1.0))
        );
        // default/contract-> contract-default/contract-default <- contract/default
        assert_eq!(
            merge_elts(vec!["merge def ctr_num", "merge ctr_id def2"]),
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
        assert_eq!(
            merge_elts(vec!["val", "merge ctr_num def"]),
            Ok(Term::Num(1.0))
        );
    }
}
