use nickel_lang::{identifier::Ident, term::Term, transform::free_vars};

use std::collections::{HashMap, HashSet};
use std::iter::IntoIterator;

use nickel_lang_utilities::parse;

// fn is_subset_of<'a>(free_vars: impl IntoIterator<Item = &'a Ident>, rec_fields: &Vec<&Ident>) -> bool {
//    free_vars.into_iter().all(|id| rec_fields.contains(&id))
// }

fn free_vars_eq(free_vars: &HashSet<Ident>, expected: Vec<&str>) -> bool {
    let expected_set: HashSet<Ident> = expected.into_iter().map(Ident::from).collect();
    *free_vars == expected_set
}

fn stat_free_vars_incl(
    stat_fields: &HashMap<Ident, HashSet<Ident>>,
    mut expected: HashMap<&str, Vec<&str>>,
) -> bool {
    stat_fields
        .iter()
        .all(|(id, set)| free_vars_eq(set, expected.remove(id.as_ref()).unwrap()))
}

fn dyn_free_vars_incl(dyn_fields: &Vec<HashSet<Ident>>, mut expected: Vec<Vec<&str>>) -> bool {
    dyn_fields
        .iter()
        .enumerate()
        .all(|(i, set)| free_vars_eq(set, std::mem::take(&mut expected[i])))
}

fn check_dyn_vars(expr: &str, expected: Vec<Vec<&str>>) -> bool {
    let mut rt = parse(expr).unwrap();
    free_vars::transform(&mut rt);

    match rt.term.into_owned() {
        Term::RecRecord(_, dyns, deps) => {
            let deps = deps.unwrap();
            dyns.len() == deps.dyn_fields.len() && dyn_free_vars_incl(&deps.dyn_fields, expected)
        }
        _ => panic!("{} hasn't been parsed as a record", expr),
    }
}

fn check_stat_vars(expr: &str, expected: HashMap<&str, Vec<&str>>) -> bool {
    let mut rt = parse(expr).unwrap();
    free_vars::transform(&mut rt);

    match rt.term.into_owned() {
        Term::RecRecord(record, _, deps) => {
            let deps = deps.unwrap();
            println!(
                "-- comparing {:#?} **AND* {:#?}",
                deps.stat_fields, expected
            );
            record.fields.len() == deps.stat_fields.len()
                && stat_free_vars_incl(&deps.stat_fields, expected)
        }
        _ => panic!("{} hasn't been parsed as a record", expr),
    }
}

#[test]
fn static_record() {
    assert!(check_stat_vars(
        "{
          a = b + 1 + h,
          b = f (a + 1) z,
          c = if a.r then [b] else {foo = c}
        }",
        HashMap::from([
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
    assert!(check_stat_vars(
        "{
          a = let b = 0 in b + a + h,
          b = let a = c in f (a + 1) z,
          c = let foo = null in let bar = null in if a.r then [b] else {foo = c}
        }",
        HashMap::from([
            ("a", vec!["a"]),
            ("b", vec!["c"]),
            ("c", vec!["a", "b", "c"])
        ])
    ));
}

#[test]
fn destruct_let() {
    assert!(check_stat_vars(
        "{
          a = let {b={foo, bar}} = null in b + a + h,
          b = let {a, b={c=e@{d}}} = null in f (a + 1 - c) z,
          c = null
        }",
        HashMap::from([("a", vec!["a", "b"]), ("b", vec!["c"]), ("c", vec![]),])
    ));
}

#[test]
fn simple_fun() {
    assert!(check_stat_vars(
        "{
          a = let f = fun a b => a + b + c in f null,
          b = (fun a => a + (fun b => b + (fun z => c))) a,
          c = [],
        }",
        HashMap::from([("a", vec!["c"]), ("b", vec!["a", "c"]), ("c", vec![]),])
    ));
}

#[test]
fn destruct_fun() {
    assert!(check_stat_vars(
        "{
          a = let f = fun {a=foo} {b=b@{}} => a + b + c in f null,
          b = (fun {a={b={a}}} => a + (fun {foo={bar=b@{b=e}}} => b + (fun z => b))) c,
          c = [],
        }",
        HashMap::from([("a", vec!["a", "c"]), ("b", vec!["c"]), ("c", vec![]),])
    ));
}

#[test]
fn nested_records() {
    assert!(check_stat_vars(
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
        HashMap::from([("a", vec!["c"]), ("b", vec!["a", "c"]), ("c", vec!["b"]),])
    ));
}

#[test]
fn recursive_let() {
    assert!(check_stat_vars(
        "{
          a = let rec b = b + a + h in b,
          b = let rec a = a + b in f (a + 1) z,
          c = let rec foo = b in let rec bar = c in if a.r then [b] else {foo = c}
        }",
        HashMap::from([
            ("a", vec!["a"]),
            ("b", vec!["b"]),
            ("c", vec!["a", "b", "c"])
        ])
    ));
}
