use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nickel_lang_funcarray::FunctionalArray;

pub fn prepend(c: &mut Criterion) {
    let big_input = [0; 10000];
    let small_input = [1; 100];
    let mut group = c.benchmark_group("collect");

    group.bench_function("prepend 10 + 10000, N=8", |b| {
        let mut big: FunctionalArray<_, 8> = FunctionalArray::collect(big_input.iter().copied());
        let small: FunctionalArray<_, 8> =
            FunctionalArray::collect(small_input[..10].iter().copied());
        b.iter(|| big.prepend(small.clone()))
    });

    group.bench_function("prepend 100 + 10000, N=8", |b| {
        let mut big: FunctionalArray<_, 8> = FunctionalArray::collect(big_input.iter().copied());
        let small: FunctionalArray<_, 8> = FunctionalArray::collect(small_input.iter().copied());
        b.iter(|| big.prepend(small.clone()))
    });

    group.bench_function("prepend 10 + 10000, N=64", |b| {
        let mut big: FunctionalArray<_, 64> = FunctionalArray::collect(big_input.iter().copied());
        let small: FunctionalArray<_, 64> =
            FunctionalArray::collect(small_input[..10].iter().copied());
        b.iter(|| big.prepend(small.clone()))
    });

    group.bench_function("prepend 100 + 10000, N=64", |b| {
        let mut big: FunctionalArray<_, 64> = FunctionalArray::collect(big_input.iter().copied());
        let small: FunctionalArray<_, 64> = FunctionalArray::collect(small_input.iter().copied());
        b.iter(|| big.prepend(small.clone()))
    });
}

pub fn iter_slice(c: &mut Criterion) {
    let input = [0; 10000];
    let mut group = c.benchmark_group("collect");

    group.bench_function("slice 100 out of 1k, N=8", |b| {
        let mut arr: FunctionalArray<_, 8> =
            FunctionalArray::collect(input[..1000].iter().copied());
        arr.slice(500, 600);
        b.iter(|| black_box(arr.iter().count()));
    });

    group.bench_function("slice 100 out of 10k, N=8", |b| {
        let mut arr: FunctionalArray<_, 8> = FunctionalArray::collect(input.iter().copied());
        arr.slice(5000, 5100);
        b.iter(|| black_box(arr.iter().count()));
    });

    group.bench_function("slice 100 out of 1k, N=64", |b| {
        let mut arr: FunctionalArray<_, 64> =
            FunctionalArray::collect(input[..1000].iter().copied());
        arr.slice(500, 600);
        b.iter(|| black_box(arr.iter().count()));
    });

    group.bench_function("slice 100 out of 10k, N=64", |b| {
        let mut arr: FunctionalArray<_, 64> = FunctionalArray::collect(input.iter().copied());
        arr.slice(5000, 5100);
        b.iter(|| black_box(arr.iter().count()));
    });
}

criterion_group!(benches, prepend, iter_slice);
criterion_main!(benches);
