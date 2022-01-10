use criterion::{criterion_group, criterion_main, Criterion};
use pprof::criterion::{Output, PProfProfiler};
use utilities::{bench, EvalMode};

fn fold_strings(c: &mut Criterion) {
    bench(
        "foldr strings 50",
        env!("CARGO_MANIFEST_DIR"),
        "lists/fold",
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
        "lists/fold",
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
        "lists/fold",
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
        "lists/fold",
        Some("right.nums"),
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn fold_lists(c: &mut Criterion) {
    bench(
        "foldr lists 50",
        env!("CARGO_MANIFEST_DIR"),
        "lists/fold",
        Some("right.lists_"),
        50,
        EvalMode::Normal,
        c,
    );
}

fn fold_lists_deep(c: &mut Criterion) {
    bench(
        "foldr deepseq lists 30",
        env!("CARGO_MANIFEST_DIR"),
        "lists/fold",
        Some("right.lists_"),
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn foldl_strings(c: &mut Criterion) {
    bench(
        "foldl strings 50",
        env!("CARGO_MANIFEST_DIR"),
        "lists/fold",
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
        "lists/fold",
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
        "lists/fold",
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
        "lists/fold",
        Some("left.nums"),
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn foldl_lists(c: &mut Criterion) {
    bench(
        "foldl lists 50",
        env!("CARGO_MANIFEST_DIR"),
        "lists/fold",
        Some("left.lists_"),
        50,
        EvalMode::Normal,
        c,
    );
}

fn foldl_lists_deep(c: &mut Criterion) {
    bench(
        "foldl deepseq lists 30",
        env!("CARGO_MANIFEST_DIR"),
        "lists/fold",
        Some("left.lists_"),
        30,
        EvalMode::DeepSeq,
        c,
    );
}

fn generate_normal(c: &mut Criterion) {
    bench(
        "generate normal 50",
        env!("CARGO_MANIFEST_DIR"),
        "lists/generate",
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
        "lists/generate",
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
        "lists/map",
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
        "lists/map",
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
        "lists/pipe",
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
        "lists/pipe",
        None,
        20,
        EvalMode::DeepSeq,
        c,
    );
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = fold_strings, fold_strings_deep, fold_nums, fold_nums_deep, fold_lists, fold_lists_deep, foldl_strings, foldl_strings_deep, foldl_nums, foldl_nums_deep, foldl_lists, foldl_lists_deep, generate_normal, generate_deepseq, map_normal, map_deepseq, pipe_normal, pipe_deepseq
}
criterion_main!(benches);
