#![cfg_attr(feature = "benchmark-ci", allow(unused_imports))]

use std::rc::Rc;

use criterion::criterion_main;
use nickel_lang_core::term::{
    array::{Array, ArrayAttrs},
    Number, RichTerm, Term,
};
use nickel_lang_utils::{bench::criterion_config, bench::EvalMode, ncl_bench_group};
use pretty::{BoxAllocator, DocBuilder, Pretty};

/// Generates a pseaudo-random Nickel array as a string.
#[cfg(not(feature = "benchmark-ci"))]
fn ncl_random_array(len: usize) -> String {
    let m = 2_u64.pow(32);
    let a = 1664525;
    let c = 1013904223;

    let mut numbers = Vec::with_capacity(len);
    let mut acc = 1337;

    for _ in 0..len {
        acc = (a * acc + c) % m;
        numbers.push(RichTerm::from(Term::Num(Number::from(acc))));
    }

    let xs = RichTerm::from(Term::Array(Array::new(numbers), ArrayAttrs::default()));
    let doc: DocBuilder<_, ()> = xs.pretty(&BoxAllocator);
    let mut out = Vec::new();
    doc.render(80, &mut out).unwrap();
    String::from_utf8(out).unwrap()
}

#[cfg(not(feature = "benchmark-ci"))]
ncl_bench_group! {
name = benches;
config = criterion_config();
{
        name = "foldr strings 50",
        path = "arrays/fold",
        subtest = "right.strings",
        args = (50),
    }, {
        name = "foldr deepseq strings 30",
        path = "arrays/fold",
        subtest = "right.strings",
        args = (30),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "foldr nums 50",
        path = "arrays/fold",
        subtest = "right.nums",
        args = (50),
    }, {
        name = "foldr deepseq nums 30",
        path = "arrays/fold",
        subtest = "right.nums",
        args = (30),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "foldr arrays 50",
        path = "arrays/fold",
        subtest = "right.arrays",
        args = (50),
    }, {
        name = "foldr deepseq arrays 30",
        path = "arrays/fold",
        subtest = "right.arrays",
        args = (30),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "foldl strings 50",
        path = "arrays/fold",
        subtest = "left.strings",
        args = (50),
    }, {
        name = "foldl deepseq strings 30",
        path = "arrays/fold",
        subtest = "left.strings",
        args = (30),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "foldl nums 50",
        path = "arrays/fold",
        subtest = "left.nums",
        args = (50),
    }, {
        name = "foldl deepseq nums 30",
        path = "arrays/fold",
        subtest = "left.nums",
        args = (30),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "foldl arrays 50",
        path = "arrays/fold",
        subtest = "left.arrays",
        args = (50),
    }, {
        name = "foldl deepseq arrays 30",
        path = "arrays/fold",
        subtest = "left.arrays",
        args = (30),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "generate normal 50",
        path = "arrays/generate",
        subtest = "checked",
        args = (50),
    }, {
        name = "generate normal unchecked 50",
        path = "arrays/generate",
        subtest = "unchecked",
        args = (50),
    }, {
        name = "generate deepseq 30",
        path = "arrays/generate",
        subtest = "checked",
        args = (30),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "map normal 50",
        path = "arrays/map",
        args = (50),
    }, {
        name = "map deepseq 30",
        path = "arrays/map",
        args = (30),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "pipe normal 20",
        path = "arrays/pipe",
        args = (20),
    }, {
        name = "pipe deepseq 20",
        path = "arrays/pipe",
        args = (20),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "sort normal",
        path = "arrays/sort",
        args = (ncl_random_array(50)),
    }, {
        name = "sum normal 50",
        path = "arrays/sum",
        args = (50),
    }, {
        name = "sum deepseq 50",
        path = "arrays/sum",
        args = (50),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "primes normal",
        path = "arrays/primes",
        args = (30),
    }, {
        name = "primes deepseq",
        path = "arrays/primes",
        args = (30),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "random normal",
        path = "arrays/random",
        args = (50),
    }, {
        name = "random deepseq",
        path = "arrays/random",
        args = (50),
        eval_mode = EvalMode::DeepSeq,
    }
}

#[cfg(feature = "benchmark-ci")]
ncl_bench_group! {
name = benches;
config = criterion_config();
{
        name = "foldr strings 50",
        path = "arrays/fold",
        subtest = "right.strings",
        args = (50),
    }, {
        name = "foldr strings 500",
        path = "arrays/fold",
        subtest = "right.strings",
        args = (500),
    }, {
        name = "foldl arrays 50",
        path = "arrays/fold",
        subtest = "left.arrays",
        args = (50),
    }, {
        name = "foldl arrays 500",
        path = "arrays/fold",
        subtest = "left.arrays",
        args = (500),
    }, {
        name = "generate normal 50",
        path = "arrays/generate",
        subtest = "checked",
        args = (50),
    }, {
        name = "generate normal 250",
        path = "arrays/generate",
        subtest = "checked",
        // Most other benchmarks have a factor of 10 between
        // the small and large sizes, but this one is slow so
        // use a factor of 5.
        args = (250),
    }, {
        name = "generate normal unchecked 200",
        path = "arrays/generate",
        subtest = "unchecked",
        args = (200),
    }, {
        name = "generate normal unchecked 1000",
        path = "arrays/generate",
        subtest = "unchecked",
        args = (1000),
    }, {
        name = "pipe normal 20",
        path = "arrays/pipe",
        args = (20),
    }, {
        name = "pipe normal 200",
        path = "arrays/pipe",
        args = (200),
    },
}

criterion_main!(benches);
