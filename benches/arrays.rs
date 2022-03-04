use criterion::{criterion_group, criterion_main, Criterion};
use nickel_lang_utilities::{bench, EvalMode};
use pprof::criterion::{Output, PProfProfiler};

fn fold_strings(c: &mut Criterion) {
    bench(
        "foldr strings 50",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("right.strings"),
        50,
        EvalMode::Normal,
        c,
    );
}

fn fold_strings_deep(c: &mut Criterion) {
    bench(
        "foldr deepseq strings 30",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("right.strings"),
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn fold_nums(c: &mut Criterion) {
    bench(
        "foldr nums 50",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("right.nums"),
        50,
        EvalMode::Normal,
        c,
    );
}

fn fold_nums_deep(c: &mut Criterion) {
    bench(
        "foldr deepseq nums 30",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("right.nums"),
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn fold_arrays(c: &mut Criterion) {
    bench(
        "foldr arrays 50",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("right.arrays"),
        50,
        EvalMode::Normal,
        c,
    );
}

fn fold_arrays_deep(c: &mut Criterion) {
    bench(
        "foldr deepseq arrays 30",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("right.arrays"),
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn foldl_strings(c: &mut Criterion) {
    bench(
        "foldl strings 50",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("left.strings"),
        50,
        EvalMode::Normal,
        c,
    );
}

fn foldl_strings_deep(c: &mut Criterion) {
    bench(
        "foldl deepseq strings 30",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("left.strings"),
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn foldl_nums(c: &mut Criterion) {
    bench(
        "foldl nums 50",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("left.nums"),
        50,
        EvalMode::Normal,
        c,
    );
}

fn foldl_nums_deep(c: &mut Criterion) {
    bench(
        "foldl deepseq nums 30",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("left.nums"),
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn foldl_arrays(c: &mut Criterion) {
    bench(
        "foldl arrays 50",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("left.arrays"),
        50,
        EvalMode::Normal,
        c,
    );
}

fn foldl_arrays_deep(c: &mut Criterion) {
    bench(
        "foldl deepseq arrays 30",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/fold",
        Some("left.arrays"),
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn generate_normal(c: &mut Criterion) {
    bench(
        "generate normal 50",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/generate",
        None,
        50,
        EvalMode::Normal,
        c,
    );
}

fn generate_deepseq(c: &mut Criterion) {
    bench(
        "generate deepseq 30",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/generate",
        None,
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn map_normal(c: &mut Criterion) {
    bench(
        "map normal 50",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/map",
        None,
        50,
        EvalMode::Normal,
        c,
    );
}

fn map_deepseq(c: &mut Criterion) {
    bench(
        "map deepseq 30",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/map",
        None,
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn pipe_normal(c: &mut Criterion) {
    bench(
        "pipe normal 20",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/pipe",
        None,
        20,
        EvalMode::Normal,
        c,
    );
}

fn pipe_deepseq(c: &mut Criterion) {
    bench(
        "pipe deepseq 20",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/pipe",
        None,
        20,
        EvalMode::DeepSeq,
        c,
    );
}

fn sort_normal(c: &mut Criterion) {
    bench(
        "sort normal",
        env!("CARGO_MANIFEST_DIR"),
        "arrays/sort",
        None,
        20,
        EvalMode::Normal,
        c,
    );
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = fold_strings, fold_strings_deep, fold_nums, fold_nums_deep, fold_arrays, fold_arrays_deep, foldl_strings, foldl_strings_deep, foldl_nums, foldl_nums_deep, foldl_arrays, foldl_arrays_deep, generate_normal, generate_deepseq, map_normal, map_deepseq, pipe_normal, pipe_deepseq, sort_normal
}
criterion_main!(benches);
