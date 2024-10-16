use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nickel_lang_vector::Vector;

pub fn collect(c: &mut Criterion) {
    let input = vec![0; 10000];
    let mut group = c.benchmark_group("collect");

    group.bench_function("ours 10000, N=8", |b| {
        b.iter(|| black_box(input.iter().copied().collect::<Vector<i32, 8>>()))
    });

    group.bench_function("ours 10000, N=32", |b| {
        b.iter(|| black_box(input.iter().copied().collect::<Vector<i32, 16>>()))
    });

    group.bench_function("ours 10000, N=64", |b| {
        b.iter(|| black_box(input.iter().copied().collect::<Vector<i32, 64>>()))
    });

    group.bench_function("rpds 10000", |b| {
        b.iter(|| black_box(input.iter().copied().collect::<rpds::Vector<i32>>()))
    });
}

pub fn count(c: &mut Criterion) {
    let input = vec![0; 10000];
    let vec8: Vector<u32, 8> = input.iter().copied().collect();
    let vec32: Vector<u32, 32> = input.iter().copied().collect();
    let vec64: Vector<u32, 64> = input.iter().copied().collect();
    let rpds: rpds::Vector<u32> = input.iter().copied().collect();
    let mut group = c.benchmark_group("iter");

    group.bench_function("ours 10000, N=8", |b| {
        b.iter(|| black_box(vec8.iter().count()));
    });

    group.bench_function("ours 10000, N=32", |b| {
        b.iter(|| black_box(vec32.iter().count()));
    });

    group.bench_function("ours 10000, N=64", |b| {
        b.iter(|| black_box(vec64.iter().count()));
    });

    group.bench_function("rpds 10000", |b| {
        b.iter(|| black_box(rpds.iter().count()));
    });
}

pub fn get(c: &mut Criterion) {
    let input = vec![0; 10000];
    let vec8: Vector<u32, 8> = input.iter().copied().collect();
    let vec32: Vector<u32, 32> = input.iter().copied().collect();
    let vec64: Vector<u32, 64> = input.iter().copied().collect();
    let rpds: rpds::Vector<u32> = input.iter().copied().collect();
    let mut group = c.benchmark_group("get");

    group.bench_function("ours 10000, N=8", |b| {
        b.iter(|| {
            for i in 0..10000 {
                black_box(vec8.get(i));
            }
        });
    });

    group.bench_function("ours 10000, N=32", |b| {
        b.iter(|| {
            for i in 0..10000 {
                black_box(vec32.get(i));
            }
        });
    });

    group.bench_function("ours 10000, N=64", |b| {
        b.iter(|| {
            for i in 0..10000 {
                black_box(vec64.get(i));
            }
        });
    });

    group.bench_function("rpds 10000", |b| {
        b.iter(|| {
            for i in 0..10000 {
                black_box(rpds.get(i));
            }
        });
    });
}

criterion_group!(benches, collect, count, get);
criterion_main!(benches);
