use criterion::{criterion_group, criterion_main, Criterion};

use utilities::{bench_args, EvalMode};

fn dummy(c: &mut Criterion) { }

// Waiting for lists stdlib
// fn count_letters(c: &mut Criterion) {
//     bench("countLetters", env!("CARGO_MANIFEST_DIR"), "records/countLetters", None,
//         vec![r#"
//             "
//             "
//         "#r,
//         EvalMode::DeepSeq, c);
// }
//
// fn merge(c: &mut Criterion) {
//     bench("merge", "records/merge", None, vec!["500", "50"], EvalMode::DeepSeq, c);
// }

//criterion_group!(benches, count_letters, merge);
criterion_group!(benches, dummy);
criterion_main!(benches);
