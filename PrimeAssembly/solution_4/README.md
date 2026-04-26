# cwager x86-64 NASM solutions

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This directory currently contains two multithreaded x86-64 NASM implementations of the base Sieve of Eratosthenes:

- `cwager_x64ff_mt.asm`
- `cwager_x64ff_mt_extreme.asm`

The intended benchmark/submission variant is `cwager_x64ff_mt_extreme.asm`, but the helper scripts and Docker flow build and run both variants for side-by-side comparison.

Both implementations:

- sieve odd candidates only
- use a packed 1-bit representation for each odd candidate
- discover every odd sieving factor from the runtime sieve state
- preserve validation against the expected `78,498` primes at `1,000,000`
- print using the drag-race output format

## Variants

### `cwager_x64ff_mt.asm`

This is the conservative baseline variant. Each worker thread:

- allocates a fresh sieve buffer dynamically at runtime for each pass
- initializes that buffer from scratch
- runs a faithful base-algorithm sieve
- frees the sieve unless it is the final completed pass retained for validation

This version matches the current repository wording for `faithful=yes` most directly.

### `cwager_x64ff_mt_extreme.asm`

This is the aggressive variant aimed at matching the high-end Rust hybrid entries while staying within the base-algorithm rules.

Key techniques:

- dense resetters for runtime-discovered odd factors up to `129`
- specialized dense mask paths for the hottest small factors
- sparse byte-pattern resetters for larger runtime-discovered factors
- per-pass dynamic sieve allocation with full reinitialization before each run

All specialized paths are entered only after the candidate factor has been discovered from the sieve bitset at runtime. No wheel, prime table, or prior knowledge of odd primes is encoded.

## Rules / classification notes

Based on the current repository rules and the maintainer clarification quoted in discussion:

- `algorithm=base`: yes for both files
- `bits=1`: yes for both files

Why `base` still applies:

- factors are sought by sequentially checking odd candidates starting at `3`
- each odd sieving factor is discovered from the runtime sieve
- clearing still projects multiples of the discovered factor onto the sieve space
- aggressive marking logic is only a projection optimization after discovery, not prior-prime knowledge

Why `bits=1` still applies:

- the sieve stores only odd candidates
- each odd candidate uses one bit in a dense bitset

### Faithfulness

Under the current wording in [`CONTRIBUTING.md`](/home/cwager/projects/Primes/CONTRIBUTING.md), both variants fit `faithful=yes`:

- the sieve state should be encapsulated in a class or closest equivalent
- each iteration should re-create a new instance from scratch
- the prime-candidate buffer should be allocated dynamically at runtime

- `cwager_x64ff_mt.asm` uses the straightforward baseline lifecycle
- `cwager_x64ff_mt_extreme.asm` now also recreates its dynamic sieve allocation on every pass, while keeping the aggressive runtime-discovered marking optimizations inside `run_sieve`

### Code-based rule confirmation

`cwager_x64ff_mt_extreme.asm` is within the requested classification for the following code-level reasons:

- `algorithm=base`:
  the outer loop scans odd factors sequentially starting at `3`, the candidate bit is tested from the runtime sieve state, and only then does the code choose a dense or sparse projection helper to clear that factor's multiples.
- `faithful=yes`:
  each worker owns the full sieve state in `worker_state`, and `worker_main` allocates a fresh dynamic sieve buffer on every pass, reinitializes it, runs the sieve, and keeps only the final completed pass for validation.
- `bits=1`:
  the implementation stores only odd candidates and uses one bit per odd candidate in the packed sieve buffer.

What the optimized jump tables do not do:

- they do not encode prior knowledge that any odd number is prime
- they do not skip candidate discovery
- they do not implement a wheel or hardcoded prime table

They are only projection helpers, selected after the candidate has been identified as prime from the runtime sieve bitset.

## Build and run

From this directory:

```bash
nasm -felf64 cwager_x64ff_mt.asm -o cwager_x64ff_mt.o
gcc -no-pie -pthread cwager_x64ff_mt.o -o cwager_x64ff_mt

nasm -felf64 cwager_x64ff_mt_extreme.asm -o cwager_x64ff_mt_extreme.o
gcc -no-pie -pthread cwager_x64ff_mt_extreme.o -o cwager_x64ff_mt_extreme
```

The default helper scripts build and run both variants:

```bash
./build.sh
./run.sh
```

Run them individually:

```bash
./cwager_x64ff_mt
./cwager_x64ff_mt_extreme
```

## Docker

Build and run the containerized comparison:

```bash
docker build -t cwager-x64ff-mt .
docker run --rm cwager-x64ff-mt
```

## Output

Example recent outputs:

```text
cwager_x64ff_mt_extreme;630045;5.000;16;algorithm=base,faithful=yes,bits=1
cwager_x64ff_mt;181358;5.000;16;algorithm=base,faithful=yes,bits=1
```

## Notes

These implementations target amd64/x86-64 Linux and link against `pthread` and `libc`.
