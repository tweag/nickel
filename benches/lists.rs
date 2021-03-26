use criterion::{criterion_group, criterion_main, Criterion};

use utilities::{bench, EvalMode};

// Waiting for lists stdlib
//fn fold_normal(c: &mut Criterion) {
//    bench("fold normal 100", env!("CARGO_MANIFEST_DIR"), "lists/fold", None, 100, EvalMode::Normal, c);
//}
//
//fn fold_deepseq(c: &mut Criterion) {
//    bench("fold deepseq 100", env!("CARGO_MANIFEST_DIR"), "lists/fold", None, 100, EvalMode::DeepSeq, c);
//}

fn generate_normal(c: &mut Criterion) {
    bench("generate normal 50", env!("CARGO_MANIFEST_DIR"), "lists/generate", None, 50, EvalMode::Normal, c);
}

fn generate_deepseq(c: &mut Criterion) {
    bench("generate deepseq 50", env!("CARGO_MANIFEST_DIR"), "lists/generate", None, 50, EvalMode::DeepSeq, c);
}

// Waiting for the lists stdlib
// fn map_normal(c: &mut Criterion) {
//     bench("map normal 100", env!("CARGO_MANIFEST_DIR"), "lists/map", None, 100, EvalMode::Normal, c);
// }
//
// fn map_deepseq(c: &mut Criterion) {
//     bench("map deepseq 100", env!("CARGO_MANIFEST_DIR"), "lists/map", None, 100, EvalMode::DeepSeq, c);
// }

//fn pipe_normal(c: &mut Criterion) {
//    bench("pipe normal 50", env!("CARGO_MANIFEST_DIR"), "lists/pipe", None, 50, EvalMode::Normal, c);
//}
//
//fn pipe_deepseq(c: &mut Criterion) {
//    bench("pipe deepseq 50", env!("CARGO_MANIFEST_DIR"), "lists/pipe", None, 50, EvalMode::DeepSeq, c);
//}

//criterion_group!(benches, fold_normal, fold_deepseq, generate_normal, generate_deepseq, map_normal, map_deepseq, pipe_normal, pipe_deepseq);
criterion_group!(benches, generate_normal, generate_deepseq);
criterion_main!(benches);
