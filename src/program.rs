use crate::eval::{eval, CallStack, EvalError, IdentKind, StackElem};
use crate::identifier::Ident;
use crate::label::{Label, TyPath};
use crate::parser;
use crate::position::SourceMapper;
use crate::term::{RichTerm, Term};
use crate::transformations;
use crate::typecheck::type_check;
use std::fs;
use std::io::{self, Read};
use std::path::Path;
use std::result::Result;

pub struct Program<T: Read> {
    src: T,
    include_contracts: bool,
    read: Option<String>,
    parsed: Option<RichTerm>,
    source_name: Option<String>,
}

impl Program<io::Stdin> {
    pub fn new_from_stdin() -> Program<io::Stdin> {
        Program::new_from_source(io::stdin(), Some(String::from("<stdin>")))
    }
}

impl Program<fs::File> {
    pub fn new_from_file<P: AsRef<Path>>(path: P) -> io::Result<Program<fs::File>> {
        let filename = path
            .as_ref()
            .to_str()
            .unwrap_or_else(|| {
                panic!("Program::new_from_file: the given path is not valid unicode")
            })
            .to_string();

        let file = fs::File::open(path)?;
        Ok(Program::new_from_source(file, Some(filename)))
    }
}

impl<T: Read> Program<T> {
    fn new_from_source(s: T, source_name: Option<String>) -> Program<T> {
        Program {
            src: s,
            include_contracts: true,
            read: None,
            parsed: None,
            source_name,
        }
    }

    pub fn eval(&mut self) -> Result<Term, String> {
        let t = self.parse()?;
        println!("Typechecked: {:?}", type_check(t.as_ref()));
        let t = transformations::share_normal_form::transform(&t);
        match eval(t) {
            Ok(t) => Ok(t),
            Err(EvalError::BlameError(l, cs)) => Err(self.process_blame(l, cs)),
            Err(EvalError::TypeError(s)) => Err(s),
        }
    }

    fn parse(&mut self) -> Result<RichTerm, String> {
        if self.parsed.is_none() {
            let mut buf = self.read()?;
            if self.include_contracts {
                // TODO get rid of this once we have imports
                buf.insert_str(0, &Self::contracts());
            }

            match parser::grammar::TermParser::new().parse(&buf) {
                Ok(t) => self.parsed = Some(t),
                Err(e) => {
                    return Err(format!("Reached the following error while parsing:\n{}", e));
                }
            };
        }

        if let Some(t) = self.parsed.clone() {
            Ok(t)
        } else {
            Err("Couldn't get the parsed term back.".to_string())
        }
    }

    fn read(&mut self) -> Result<String, String> {
        if self.read.is_none() {
            let mut buffer = String::new();
            match self.src.read_to_string(&mut buffer) {
                Ok(_) => self.read = Some(buffer),
                Err(e) => {
                    return Err(format!(
                        "Reached the following error while reading the file: '{}'",
                        e
                    ));
                }
            }
        }

        if let Some(b) = self.read.clone() {
            Ok(b)
        } else {
            Err("Couldn't get the file back.".to_string())
        }
    }

    fn process_blame(&mut self, l: Label, cs_opt: Option<CallStack>) -> String {
        let source_name = self
            .source_name
            .clone()
            .unwrap_or(String::from("<unknown>"));
        let mapper = SourceMapper::new_with_offset(
            source_name,
            self.read
                .as_ref()
                .unwrap_or_else(|| {
                    panic!("Program::process_blame: expected program buffer (read) to be loaded")
                })
                .as_str(),
            Self::contracts().len(),
        );

        let mut s = String::new();
        s.push_str("Reached a blame label, some cast went terribly wrong\n");
        s.push_str("    Tag:\n");
        s.push_str(&l.tag);
        s.push_str("\n");

        if let Some((left, right)) = mapper.map_span(l.l, l.r) {
            if left.line == right.line {
                s.push_str(&format!(
                    "    Line: {} Columns: {} to {}\n",
                    left.line, left.column, right.column
                ));
            } else {
                s.push_str(&format!(
                    "    Line: {} Column: {}\n",
                    left.line, left.column
                ));
                s.push_str(&format!(
                    "    to Line: {} Column: {}\n",
                    right.line, right.column
                ));
            }
        }

        s.push_str(&format!("    Polarity: {}\n", l.polarity));
        if l.polarity {
            s.push_str("    The blame is on the value (positive blame)\n");
        } else {
            s.push_str("    The blame is on the context (negative blame)\n");
        }
        if l.path != TyPath::Nil() {
            s.push_str(&format!("    Path: {:?}\n", l.path));
        }

        if let Some(cs) = cs_opt {
            s.push_str("\nCallStack:\n=========\n");
            s = Program::<T>::show_call_stack(&mapper, s, cs);
        }
        s
    }

    fn show_call_stack(mapper: &SourceMapper, mut s: String, mut cs: CallStack) -> String {
        for e in cs.drain(..).rev() {
            match e {
                StackElem::App(Some((_l, _r))) => {
                    // I'm not sure this App stack is really useful,
                    // will leave it hanging for now
                    //
                    // if let Some((linef, colf)) = self.get_line_and_col(l) {
                    //     s.push_str(&format!(
                    //         "    Applied to a term on line: {} col: {}\n",
                    //         linef, colf
                    //     ));
                    // }
                }
                StackElem::Var(IdentKind::Let(), Ident(x), Some((left, _))) => {
                    if let Some(left) = mapper.map_pos(left) {
                        s.push_str(&format!(
                            "On a call to {} on line: {} col: {}\n",
                            x, left.line, left.column
                        ));
                    }
                }
                StackElem::Var(IdentKind::Lam(), Ident(x), Some((left, _))) => {
                    if let Some(left) = mapper.map_pos(left) {
                        s.push_str(&format!(
                            "    Bound to {} on line: {} col: {}\n",
                            x, left.line, left.column
                        ));
                    }
                }
                _ => {}
            }
        }
        s
    }

    fn contracts() -> String {
        "let dyn = fun l => fun t => t in

        let num = fun l => fun t => if isNum t then t else blame (tag[num] l) in

        let bool = fun l => fun t => if isBool t then t else blame (tag[bool] l) in

        let string = fun l => fun t => if isStr t then t else blame (tag[str] l) in

        let list = fun l => fun t => if isList t then t else blame (tag[list] l) in

        let func = fun s => fun t => fun l => fun e => 
  let l = tag[func] l in if isFun e then (fun x => t (goCodom l) (e (s (chngPol (goDom l)) x))) else blame l in

  let forall_var = fun sy => fun pol => fun l => fun t => let lPol = polarity l in 
if pol =b lPol then
  unwrap sy t (blame (tag[unwrp] l))
else
  wrap sy t
in

let fail = fun l => fun t => blame (tag[fail] l) in

let row_extend = fun contr => fun case => fun l => fun t => 
        if (case t) then t else contr (tag[NotRowExt] l) t
in
".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    fn eval_string(s: &str) -> Result<Term, String> {
        let src = Cursor::new(s);

        let mut p = Program::new_from_source(src, None);
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
            "let bool = fun l => fun t => if isBool t then t else blame l in

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
            "let alwaysTrue = fun l => fun t => let boolT = Assume(Bool, t) in 
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
            "let alwaysTrue = fun l => fun t => let boolT = Assume(Bool, t) in 
    if boolT then boolT else blame l in
let alwaysFalse = fun l => fun t => let boolT = Assume(Bool, t) in 
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
                    (fun y => fun x => if isNum x then x + 1 else 0) 
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
        eval_string("merge {a=1} {a=2}").unwrap();
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
}
