# Standalone search simulator

This workspace is the first foundation for a fast, standalone Rust Netrunner search
simulator. It defines contracts, trace interchange, the System Gateway beginner card
pool, and state-layout measurements. It does **not** yet implement card behavior, the
rules engine, MCTS, an intermediate Gateway rules layer, networking to Jinteki, or a
production server.

Supported rules must be exact. A later engine will automatically drain deterministic
work and stop only at a decision, chance, terminal, or structured unsupported boundary;
it must never approximate an unsupported card or mechanic.

## Crates and boundaries

- `netrunner-sim-core`: serde-free, allocation-conscious Rust contracts and state
  primitives used by inner search. It owns compact IDs, semantic actions, timing/pass
  state, perspective-safe card references, exact chance weights, and boundaries.
- `netrunner-sim-protocol`: versioned serde/JSONL adapters and semantic cross-engine
  traces. Serialization stays outside the expansion loop. Tagged action variants use a
  neutral `data` payload field so future non-ability payloads do not inherit a misleading
  wire label.
- `netrunner-sim-gateway`: checked-in System Gateway data needed by the initial matchup.
  Its runtime does not require Clojure or a JVM.
- `netrunner-sim-server`: a compiling process-boundary placeholder. JSON-RPC is not part
  of the 100k expansions/second KPI; inner search will call Rust APIs directly.

States are designed to be immutable/forkable for independent workers. Core will not own
a shared mutable transposition table. Paid windows retain a timing-window identifier and
sequential Corp/Runner pass state rather than collapsing both players' passes.

Hidden cards use opaque perspective tokens, so visible action descriptors do not require
or reveal a `CardDefId`. The chance contract can carry correlated latent multisets and
defer expansion into exact rational weighted outcomes. Both Corp and Runner perspectives
are first-class. Deterministic core code has no implicit RNG: callers either select an
explicit outcome or record seeded sampling telemetry in protocol traces.

Cross-engine comparison uses normalized observations and separately labeled Corp and
Runner observable hashes. It intentionally does not compare current Clojure replay diffs.

## Commands

Run from `simulator/`:

```sh
cargo fmt --all --check
cargo clippy --workspace --all-targets --all-features -- -D warnings
cargo test --workspace
cargo bench -p netrunner-sim-core --bench state_layout
```

Regenerate or drift-check the manifest from the repository root after fetching canonical
card data:

```sh
lein fetch --no-db --no-card-images
lein simulator-gateway
lein simulator-gateway --check
```

The checked-in beginner definition currently follows
`src/cljc/jinteki/preconstructed.cljc` exactly: Corp has 34 cards and Runner has 30,
across 32 unique deck cards, plus The Syndicate and The Catalyst identities. The target
is 6 agenda points. This calls out the source's 34-card Corp list explicitly rather than
silently changing quantities.

## State-layout spike

`state_layout` compares a straightforward owned flat state with `Arc<Vec<_>>`
copy-on-write pages for opening (94 cards) and midgame-like (150 cards) synthetic states.
It measures a fork alone and a fork followed by eight representative scalar/card
mutations, including the first COW materialization cost. Criterion reports sampled
distributions; results are machine-specific and are not a portable p95 guarantee.

Observed on Apple Silicon with Rust 1.85.0:

| Scenario | Owned flat | Arc COW |
| --- | ---: | ---: |
| Opening fork | 49.6-50.1 ns | 4.71-4.72 ns |
| Opening fork + 8 mutations | 53.7-54.2 ns | 80.5-80.9 ns |
| Midgame fork | 51.2-51.6 ns | 4.71-4.72 ns |
| Midgame fork + 8 mutations | 54.7-55.2 ns | 86.0-86.4 ns |

Both fork strategies are far below one microsecond in this run. The owned flat layout is
the simpler representation and is faster once representative mutations are included, so
it is the foundation choice. COW pages and generic persistent collections are not
justified by this spike.
