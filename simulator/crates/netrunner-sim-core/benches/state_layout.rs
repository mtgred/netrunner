use std::hint::black_box;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use netrunner_sim_core::state_layout::{CowPagedState, OwnedFlatState, SyntheticStateSize};

fn benchmark_state_layouts(criterion: &mut Criterion) {
    for (label, size) in [
        ("opening", SyntheticStateSize::Opening),
        ("midgame", SyntheticStateSize::Midgame),
    ] {
        let owned = OwnedFlatState::synthetic(size);
        let cow = CowPagedState::synthetic(size);

        let mut forks = criterion.benchmark_group(format!("state_fork/{label}"));
        forks.throughput(Throughput::Elements(1));
        forks.bench_with_input(
            BenchmarkId::new("owned_flat", label),
            &owned,
            |bench, state| {
                bench.iter(|| black_box(state.clone()));
            },
        );
        forks.bench_with_input(BenchmarkId::new("arc_cow", label), &cow, |bench, state| {
            bench.iter(|| black_box(state.clone()));
        });
        forks.finish();

        let mut mutations = criterion.benchmark_group(format!("fork_and_apply_8/{label}"));
        mutations.throughput(Throughput::Elements(8));
        mutations.bench_with_input(
            BenchmarkId::new("owned_flat", label),
            &owned,
            |bench, state| {
                bench.iter(|| {
                    let mut fork = state.clone();
                    for index in 0..8 {
                        fork.apply_like_mutation(black_box(index));
                    }
                    black_box(fork);
                });
            },
        );
        mutations.bench_with_input(BenchmarkId::new("arc_cow", label), &cow, |bench, state| {
            bench.iter(|| {
                let mut fork = state.clone();
                for index in 0..8 {
                    fork.apply_like_mutation(black_box(index));
                }
                black_box(fork);
            });
        });
        mutations.finish();
    }
}

criterion_group!(benches, benchmark_state_layouts);
criterion_main!(benches);
