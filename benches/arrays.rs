use criterion::{criterion_main, Criterion};
use nickel_lang_utilities::{ncl_bench_group, EvalMode};
use pprof::criterion::{Output, PProfProfiler};

ncl_bench_group! {
name = benches;
config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
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
        args = (20),
    }, {
        name = "sum normal 50",
        path = "arrays/sum",
        args = (50),
    }, {
        name = "sum deepseq 50",
        path = "arrays/sum",
        args = (50),
        eval_mode = EvalMode::DeepSeq,
    }
}
criterion_main!(benches);
