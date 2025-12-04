use super::{
    cache::{Cache as _, CacheImpl},
    *,
};
use crate::{
    cache::resolvers::{DummyResolver, SimpleResolver},
    error::{ImportError, ImportErrorKind, NullReporter},
    files::Files,
    label::Label,
    parser::{ErrorTolerantParserCompat, grammar, lexer},
    term::{BinaryOp, Number, StrChunk, UnaryOp, make as mk_term},
    transform::import_resolution::strict::resolve_imports,
    {mk_app, mk_fun, mk_record},
};
use assert_matches::assert_matches;

/// Generates a minimal VM context for test purpose.
fn vm_ctxt() -> VmContext<DummyResolver, CacheImpl> {
    VmContext::new(DummyResolver {}, std::io::sink(), NullReporter {})
}

/// Creates an non-unwinding machine with an empty initial environment from a VM contex.
fn new_no_unwind_vm<R: ImportResolver, C: cache::Cache>(
    vm_ctxt: &mut VmContext<R, C>,
) -> NoUnwindVirtualMachine<'_, R, C> {
    NoUnwindVirtualMachine::new(VirtualMachine::new_empty_env(vm_ctxt))
}

/// Evaluate a term without import support.
fn eval_no_import(v: NickelValue) -> Result<NickelValue, EvalError> {
    new_no_unwind_vm(&mut vm_ctxt()).eval(v)
}

/// Fully evaluate a term without import support.
fn eval_full_no_import(v: NickelValue) -> Result<NickelValue, EvalError> {
    new_no_unwind_vm(&mut vm_ctxt()).eval_full(v)
}

fn parse(s: &str) -> Option<NickelValue> {
    parse_with_table(&mut PosTable::new(), s)
}

/// Same as [parse], but when we actually need the position table later.
fn parse_with_table(pos_table: &mut PosTable, s: &str) -> Option<NickelValue> {
    let id = Files::empty().add("<test>", String::from(s));

    grammar::TermParser::new()
        .parse_strict_compat(pos_table, id, lexer::Lexer::new(s))
        .map(NickelValue::without_pos)
        .map_err(|err| println!("{err:?}"))
        .ok()
}

#[test]
fn identity_over_values() {
    let num = NickelValue::number_posless(Number::try_from(45.3).unwrap());
    assert_eq!(Ok(num.clone()), eval_no_import(num));

    let boolean = NickelValue::bool_true();
    assert_eq!(Ok(boolean.clone()), eval_no_import(boolean));

    let lambda = mk_fun!("x", mk_app!(mk_term::var("x"), mk_term::var("x")));
    assert_eq!(Ok(lambda.clone()), eval_no_import(lambda));
}

#[test]
fn blame_panics() {
    let dummy = Label::dummy();
    let res = eval_no_import(mk_term::op1(
        UnaryOp::Blame,
        NickelValue::label_posless(dummy.clone()),
    ));

    if let Err(data) = res
        && let EvalErrorKind::BlameError {
            evaluated_arg: _,
            label,
        } = data.error
    {
        assert_eq!(dummy, label);
    } else {
        panic!("This evaluation should've returned a BlameError!");
    }
}

#[test]
#[should_panic]
fn lone_var_panics() {
    eval_no_import(mk_term::var("unbound")).unwrap();
}

#[test]
fn only_fun_are_applicable() {
    eval_no_import(mk_app!(NickelValue::bool_true(), mk_term::integer(45))).unwrap_err();
}

#[test]
fn simple_app() {
    let t = mk_app!(mk_term::id(), mk_term::integer(5));
    assert_eq!(Ok(NickelValue::number_posless(5)), eval_no_import(t));
}

#[test]
fn simple_let() {
    let t = mk_term::let_one_in("x", mk_term::integer(5), mk_term::var("x"));
    assert_eq!(Ok(NickelValue::number_posless(5)), eval_no_import(t));
}

#[test]
fn simple_ite() {
    let t = mk_term::if_then_else(
        NickelValue::bool_true(),
        mk_term::integer(5),
        NickelValue::bool_false(),
    );
    assert_eq!(Ok(NickelValue::number_posless(5)), eval_no_import(t));
}

#[test]
fn simple_plus() {
    let t = mk_term::op2(
        BinaryOp::Plus,
        mk_term::integer(5),
        NickelValue::number_posless(Number::try_from(7.5).unwrap()),
    );
    assert_eq!(
        Ok(NickelValue::number_posless(Number::try_from(12.5).unwrap())),
        eval_no_import(t)
    );
}

#[test]
fn asking_for_various_types() {
    let num = mk_term::op1(
        UnaryOp::Typeof,
        NickelValue::number_posless(Number::try_from(45.3).unwrap()),
    );
    assert_eq!(
        Ok(NickelValue::enum_tag_posless("Number")),
        eval_no_import(num)
    );

    let boolean = mk_term::op1(UnaryOp::Typeof, NickelValue::bool_true());
    assert_eq!(
        Ok(NickelValue::enum_tag_posless("Bool")),
        eval_no_import(boolean)
    );

    let lambda = mk_term::op1(
        UnaryOp::Typeof,
        mk_fun!("x", mk_app!(mk_term::var("x"), mk_term::var("x"))),
    );
    assert_eq!(
        Ok(NickelValue::enum_tag_posless("Function")),
        eval_no_import(lambda)
    );
}

#[test]
fn imports() {
    let mut vm_ctxt: VmContext<_, CacheImpl> =
        VmContext::new(SimpleResolver::new(), std::io::sink(), NullReporter {});

    vm_ctxt
        .import_resolver
        .add_source(String::from("two"), String::from("1 + 1"));
    vm_ctxt
        .import_resolver
        .add_source(String::from("lib"), String::from("{f = true}"));
    vm_ctxt
        .import_resolver
        .add_source(String::from("bad"), String::from("^$*/.23ab 0°@"));
    vm_ctxt.import_resolver.add_source(
        String::from("nested"),
        String::from("let x = import \"two\" as 'Nickel in x + 1"),
    );
    vm_ctxt.import_resolver.add_source(
        String::from("cycle"),
        String::from("let x = import \"cycle_b\" as 'Nickel in {a = 1, b = x.a}"),
    );
    vm_ctxt.import_resolver.add_source(
        String::from("cycle_b"),
        String::from("let x = import \"cycle\" as 'Nickel in {a = x.a}"),
    );

    fn mk_import<R>(
        vm_ctxt: &mut VmContext<R, CacheImpl>,
        var: &str,
        import: &str,
        body: NickelValue,
    ) -> Result<NickelValue, ImportError>
    where
        R: ImportResolver,
    {
        resolve_imports(
            &mut vm_ctxt.pos_table,
            mk_term::let_one_in(
                var,
                mk_term::import(import, crate::cache::InputFormat::Nickel),
                body,
            ),
            &mut vm_ctxt.import_resolver,
        )
        .map(|resolve_result| resolve_result.transformed_term)
    }

    // let x = import "does_not_exist" in x
    match *mk_import(&mut vm_ctxt, "x", "does_not_exist", mk_term::var("x")).unwrap_err() {
        ImportErrorKind::IOError(_, _, _) => (),
        _ => panic!(),
    };

    // let x = import "bad" in x
    match *mk_import(&mut vm_ctxt, "x", "bad", mk_term::var("x")).unwrap_err() {
        ImportErrorKind::ParseErrors(_, _) => (),
        _ => panic!(),
    };

    // let x = import "two" in x
    let mk_import_two = mk_import(&mut vm_ctxt, "x", "two", mk_term::var("x")).unwrap();

    assert_eq!(
        new_no_unwind_vm(&mut vm_ctxt)
            .eval(mk_import_two)
            .map(NickelValue::without_pos)
            .unwrap(),
        mk_term::integer(2)
    );

    // let x = import "lib" in x.f
    let mk_import_lib = mk_import(
        &mut vm_ctxt,
        "x",
        "lib",
        mk_term::op1(
            UnaryOp::RecordAccess(LocIdent::from("f")),
            mk_term::var("x"),
        ),
    );

    assert_eq!(
        new_no_unwind_vm(&mut vm_ctxt)
            .eval(mk_import_lib.unwrap())
            .unwrap(),
        NickelValue::bool_true()
    );
}

#[test]
fn interpolation_simple() {
    let mut chunks = vec![
        StrChunk::Literal(String::from("Hello")),
        StrChunk::expr(mk_term::op2(
            BinaryOp::StringConcat,
            NickelValue::string_posless(", "),
            NickelValue::string_posless("World!"),
        )),
        StrChunk::Literal(String::from(" How")),
        StrChunk::expr(mk_term::if_then_else(
            NickelValue::bool_true(),
            NickelValue::string_posless(" are"),
            NickelValue::string_posless(" is"),
        )),
        StrChunk::Literal(String::from(" you?")),
    ];
    chunks.reverse();

    assert_eq!(
        eval_no_import(NickelValue::term_posless(Term::StrChunks(chunks))),
        Ok(NickelValue::string_posless("Hello, World! How are you?"))
    );
}

#[test]
fn interpolation_nested() {
    let mut inner_chunks = vec![
        StrChunk::Literal(String::from(" How")),
        StrChunk::expr(
            Term::op2(
                BinaryOp::StringConcat,
                NickelValue::string_posless(" ar"),
                NickelValue::string_posless("e"),
            )
            .into(),
        ),
        StrChunk::expr(mk_term::if_then_else(
            NickelValue::bool_true(),
            NickelValue::string_posless(" you"),
            NickelValue::string_posless(" me"),
        )),
    ];
    inner_chunks.reverse();

    let mut chunks = vec![
        StrChunk::Literal(String::from("Hello, World!")),
        StrChunk::expr(Term::StrChunks(inner_chunks).into()),
        StrChunk::Literal(String::from("?")),
    ];
    chunks.reverse();

    assert_eq!(
        eval_no_import(NickelValue::term_posless(Term::StrChunks(chunks))),
        Ok(NickelValue::string_posless("Hello, World! How are you?"))
    );
}

#[test]
fn initial_env() {
    let mut initial_env = Environment::new();
    let mut eval_cache = CacheImpl::new();
    initial_env.insert(
        Ident::from("g"),
        eval_cache.add(mk_term::integer(1).into(), BindingType::Normal),
    );

    let t = mk_term::let_one_in("x", mk_term::integer(2), mk_term::var("x"));

    let mut vm_ctxt: VmContext<_, CacheImpl> =
        VmContext::new(DummyResolver {}, std::io::sink(), NullReporter {});

    let mut vm = NoUnwindVirtualMachine::new(
        VirtualMachine::new_empty_env(&mut vm_ctxt).with_initial_env(initial_env.clone()),
    );

    assert_eq!(
        vm.eval(t).map(NickelValue::without_pos),
        Ok(mk_term::integer(2))
    );

    let t = mk_term::let_one_in("x", mk_term::integer(2), mk_term::var("g"));

    assert_eq!(
        vm.eval(t).map(NickelValue::without_pos),
        Ok(mk_term::integer(1))
    );

    // Shadowing of the initial environment
    let t = mk_term::let_one_in("g", mk_term::integer(2), mk_term::var("g"));

    assert_eq!(
        vm.eval(t).map(NickelValue::without_pos),
        Ok(mk_term::integer(2))
    );
}

fn mk_env(bindings: Vec<(&str, NickelValue)>, eval_cache: &mut CacheImpl) -> Environment {
    bindings
        .into_iter()
        .map(|(id, t)| (id.into(), eval_cache.add(t.into(), BindingType::Normal)))
        .collect()
}

#[test]
fn substitution() {
    let mut eval_cache = CacheImpl::new();
    let mut pos_table = PosTable::new();

    let initial_env = mk_env(
        vec![
            ("glob1", mk_term::integer(1)),
            ("glob2", parse("\"Glob2\"").unwrap()),
            ("glob3", NickelValue::bool_false()),
        ],
        &mut eval_cache,
    );
    let env = mk_env(
        vec![
            ("loc1", NickelValue::bool_true()),
            ("loc2", parse("if glob3 then glob1 else glob2").unwrap()),
        ],
        &mut eval_cache,
    );

    let t = parse_with_table(
        &mut pos_table,
        "let x = 1 in if loc1 then 1 + loc2 else glob3",
    )
    .unwrap();
    assert_eq!(
        subst(&pos_table, &eval_cache, t, &initial_env, &env),
        parse_with_table(
            &mut pos_table,
            "let x = 1 in if true then 1 + (if false then 1 else \"Glob2\") else false"
        )
        .unwrap()
    );
}

#[test]
fn foreign_id() {
    let t = mk_term::op2(
        BinaryOp::Merge(Label::default().into()),
        mk_record!(("a", NickelValue::number_posless(1))),
        mk_record!(("b", NickelValue::foreign_id_posless(42))),
    );

    let evaled = eval_no_import(t.clone()).unwrap();
    // Terms that include foreign ids can be manipulated normally, and the ids are passed through.
    let container = &evaled.as_record().unwrap();

    let field = container.get("b".into()).unwrap();
    assert_matches!(field.value.as_ref().unwrap().as_foreign_id().unwrap(), 42);

    // Foreign ids cannot be compared for equality.
    let t_eq = mk_term::op2(
        BinaryOp::Eq,
        NickelValue::foreign_id_posless(43),
        NickelValue::foreign_id_posless(42),
    );
    assert_matches!(
        eval_no_import(t_eq),
        Err(data) if matches!(data.error, EvalErrorKind::IncomparableValues { .. })
    );

    // Opaque values cannot be merged (even if they're equal, since they can't get compared for equality).
    let t_merge = mk_term::op2(
        BinaryOp::Merge(Label::default().into()),
        t.clone(),
        t.clone(),
    );
    assert_matches!(
        eval_full_no_import(t_merge),
        Err(data) if matches!(data.error, EvalErrorKind::MergeIncompatibleArgs { .. })
    );

    let t_typeof = mk_term::op1(UnaryOp::Typeof, NickelValue::foreign_id_posless(42));
    let ty = eval_no_import(t_typeof).unwrap();
    let fid = LocIdent::from(Ident::new("ForeignId"));
    assert_matches!(ty.as_enum_tag(), Some(f) if f == fid);
}
