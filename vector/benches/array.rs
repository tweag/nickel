use criterion::{Criterion, black_box, criterion_group, criterion_main};
use nickel_lang_vector::Slice;

pub fn iter_slice(c: &mut Criterion) {
    let input = [0; 10000];
    let mut group = c.benchmark_group("collect");

    group.bench_function("slice 100 out of 1k, N=8", |b| {
        let mut arr: Slice<_, 8> = input[..1000].iter().copied().collect();
        arr.slice(500, 600);
        b.iter(|| black_box(arr.iter().count()));
    });

    group.bench_function("slice 100 out of 10k, N=8", |b| {
        let mut arr: Slice<_, 8> = input.iter().copied().collect();
        arr.slice(5000, 5100);
        b.iter(|| black_box(arr.iter().count()));
    });

    group.bench_function("slice 100 out of 1k, N=64", |b| {
        let mut arr: Slice<_, 64> = input[..1000].iter().copied().collect();
        arr.slice(500, 600);
        b.iter(|| black_box(arr.iter().count()));
    });

    group.bench_function("slice 100 out of 10k, N=64", |b| {
        let mut arr: Slice<_, 64> = input.iter().copied().collect();
        arr.slice(5000, 5100);
        b.iter(|| black_box(arr.iter().count()));
    });
}

criterion_group!(benches, iter_slice);
criterion_main!(benches);
