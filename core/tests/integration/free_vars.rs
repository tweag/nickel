use nickel_lang_core::{
    identifier::Ident,
    term::{record::FieldDeps, IndexMap, Term},
    transform::free_vars,
};

use std::{collections::HashSet, iter::IntoIterator, rc::Rc};

use nickel_lang_utils::test_program::parse;

fn free_vars_eq(free_vars: &FieldDeps, expected: Vec<&str>) -> bool {
    let expected_set: HashSet<Ident> = expected.into_iter().map(Ident::from).collect();
    *free_vars == FieldDeps::Known(Rc::new(expected_set))
}

/// Check that the dependencies of the static fields (including include expressions) of a record
/// are a subset of the expected ones. The matches are removed from `expected`.
fn stat_free_vars_subset(
    stat_fields: &IndexMap<Ident, FieldDeps>,
    expected: &mut IndexMap<&str, Vec<&str>>,
) -> bool {
    stat_fields
        .iter()
        .all(|(id, set)| free_vars_eq(set, expected.swap_remove(id.label()).unwrap()))
}

/// Check that the dependencies of the dynamic fields of a record are a subset of the expected
/// ones. The match are removed from `expected`.
fn dyn_free_vars_subset(dyn_fields: &[FieldDeps], mut expected: Vec<Vec<&str>>) -> bool {
    dyn_fields
        .iter()
        .enumerate()
        .all(|(i, set)| free_vars_eq(set, std::mem::take(&mut expected[i])))
}

/// Takes a Nickel expression as a string, parses it as a record and checks that the dependencies
/// of each dynamic field match the ones given in the `expected` argument.
fn check_dyn_vars(expr: &str, expected: Vec<Vec<&str>>) -> bool {
    let mut rt = parse(expr).unwrap();
    free_vars::transform(&mut rt);

    match rt.term.into_owned() {
        Term::RecRecord(_, _, dyns, deps) => {
            let deps = deps.unwrap();
            dyns.len() == deps.dyn_fields.len() && dyn_free_vars_subset(&deps.dyn_fields, expected)
        }
        _ => panic!("{expr} hasn't been parsed as a record"),
    }
}

/// Takes a string expression, parses it as a record and checks that the dependencies of each
/// static field and included field match the ones given in the `expected` argument.
fn check_stat_and_includes(expr: &str, mut expected: IndexMap<&str, Vec<&str>>) -> bool {
    let mut rt = parse(expr).unwrap();
    free_vars::transform(&mut rt);

    match rt.term.into_owned() {
        Term::RecRecord(record, includes, _, deps) => {
            let deps = deps.unwrap();
            println!(
                "-- comparing {:#?} **AND* {:#?}",
                deps.stat_fields, expected
            );
            stat_free_vars_subset(&deps.stat_fields, &mut expected)
            && record.fields.len() + includes.len() == deps.stat_fields.len()
            // the inclusion test functions `xxx_free_vars_incl` above take the free variables out
            // of expected as they are matched, so we expect that at the end of the test,
            // `expected` is empty (there was no extra field specified in `expected`)
            && expected.is_empty()
        }
        _ => panic!("{expr} hasn't been parsed as a record"),
    }
}

#[test]
fn static_record() {
    assert!(check_stat_and_includes(
        "{
          a = b + 1 + h,
          b = f (a + 1) z,
          c = if a.r then [b] else {foo = c}
        }",
        IndexMap::from([
            ("a", vec!["b"]),
            ("b", vec!["a"]),
            ("c", vec!["a", "b", "c"])
        ])
    ));
}

#[test]
fn dynamic_record() {
    assert!(check_dyn_vars(
        "{
          a = null,
          b = null,
          \"%{\"a_dyn\"}\" = b + 1 + h,
          \"%{\"b_dyn\"}\" = f (a + 1) z,
          \"%{\"c\"}\" = if a.r then [b] else {foo = c, bar.booz = a_dyn}
        }",
        vec![vec!["b"], vec!["a"], vec!["a", "b"]]
    ));
}

#[test]
fn simple_let() {
    assert!(check_stat_and_includes(
        "{
          a = let b = 0 in b + a + h,
          b = let a = c in f (a + 1) z,
          c = let foo = null in let bar = null in if a.r then [b] else {foo = c}
        }",
        IndexMap::from([
            ("a", vec!["a"]),
            ("b", vec!["c"]),
            ("c", vec!["a", "b", "c"])
        ])
    ));
}

#[test]
fn destruct_let() {
    assert!(check_stat_and_includes(
        "{
          a = let {b={foo, bar}} = null in b + a + h,
          b = let {a, b={c=e@{d}}} = null in f (a + 1 - c) z,
          c = null
        }",
        IndexMap::from([("a", vec!["a", "b"]), ("b", vec!["c"]), ("c", vec![]),])
    ));
}

#[test]
fn simple_fun() {
    assert!(check_stat_and_includes(
        "{
          a = let f = fun a b => a + b + c in f null,
          b = (fun a => a + (fun b => b + (fun z => c))) a,
          c = [],
        }",
        IndexMap::from([("a", vec!["c"]), ("b", vec!["a", "c"]), ("c", vec![]),])
    ));
}

#[test]
fn destruct_fun() {
    assert!(check_stat_and_includes(
        "{
          a = let f = fun {a=foo} {b=b@{}} => a + b + c in f null,
          b = (fun {a={b={a}}} => a + (fun {foo={bar=b@{b=e}}} => b + (fun z => b))) c,
          c = [],
        }",
        IndexMap::from([("a", vec!["a", "c"]), ("b", vec!["c"]), ("c", vec![]),])
    ));
}

#[test]
fn nested_records() {
    assert!(check_stat_and_includes(
        "{
          a = {
            foo = {bar = a},
            a = 2 + c,
          },
          b = {
            bar = a,
            baz = {b = b},
            baz.e = c,
          },
          c = {
            baz.b = b,
            baz.e = b,
          },
        }",
        IndexMap::from([("a", vec!["c"]), ("b", vec!["a", "c"]), ("c", vec!["b"]),])
    ));
}

#[test]
fn recursive_let() {
    assert!(check_stat_and_includes(
        "{
          a = let rec b = b + a + h in b,
          b = let rec a = a + b in f (a + 1) z,
          c = let rec foo = b in let rec bar = c in if a.r then [b] else {foo = c}
        }",
        IndexMap::from([
            ("a", vec!["a"]),
            ("b", vec!["b"]),
            ("c", vec!["a", "b", "c"])
        ])
    ));
}

#[test]
fn include_exprs() {
    assert!(check_stat_and_includes(
        "{
          a,
          include [b],
          c = a + b + c + d,
          include x | y,
          include z | C,
          C,
        }",
        IndexMap::from([
            ("a", vec![]),
            ("b", vec![]),
            ("c", vec!["a", "b", "c"]),
            ("x", vec![]),
            ("z", vec!["C"]),
            ("C", vec![]),
        ])
    ));
}
