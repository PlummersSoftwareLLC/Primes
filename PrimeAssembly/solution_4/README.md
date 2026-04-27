# cwager_x64ff_mt

# cwager x86-64 NASM solutions

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This directory contains two multithreaded x86-64 NASM implementations of the base Sieve of Eratosthenes:

- `cwager_x64ff_mt.asm`
- `cwager_x64ff_mt_extreme.asm`

The intended benchmark/submission variant is `cwager_x64ff_mt_extreme.asm`, but the helper scripts and Docker flow build and run both variants for side-by-side comparison.

Both implementations:

- sieve odd candidates only
- use a packed 1-bit representation for each odd candidate
- allocate and initialize a fresh sieve buffer dynamically for each timed pass
- discover every odd sieving factor from the runtime sieve state
- validate against the expected `78,498` primes at `1,000,000`
- print using the drag-race output format

## Variants

### `cwager_x64ff_mt.asm`

This is the conservative baseline variant. Each worker thread:

- allocates a fresh sieve buffer dynamically at runtime for each pass
- initializes that buffer from scratch
- runs a faithful base-algorithm sieve over odd candidates only
- frees the sieve unless it is the final completed pass retained for validation

This version follows the repository wording for `faithful=yes` most directly.

### `cwager_x64ff_mt_extreme.asm`

This is the aggressive performance-oriented variant.

Key techniques:

- contiguous dense dispatch for every runtime-discovered odd skip from `3` through `129`
- dense handlers for composite odd skips as well as prime odd skips within that range
- sparse periodic resetters for larger runtime-discovered odd skips
- sparse dispatch based on modulo-16 residue classes
- per-pass dynamic sieve allocation with full reinitialization before each run

The optimized marking paths are selected only after the candidate factor has been found to still be set in the runtime sieve bitset. The implementation does not embed a wheel, prime table, or precomputed list of odd primes.

## Rules / classification notes

Expected classification for both variants:

- `algorithm=base`: yes
- `faithful=yes`: yes
- `bits=1`: yes

### Why `base` applies

Both variants scan odd candidate factors sequentially, starting at `3`. Each candidate is tested against the runtime sieve state before any marking helper is selected.

Once a candidate factor has been discovered from the sieve bitset, the code clears projected multiples of that factor from the sieve space. The optimized dense and sparse helpers are projection optimizations after runtime discovery, not prior-prime knowledge.

The extreme variant's dense dispatch table covers the full contiguous odd skip range from `3` through `129`, including composite values such as `9`, `15`, `21`, and `25`. Composite handlers are present in the table, even though they are normally not reached because those candidates have already been cleared by earlier factors.

### Why `faithful=yes` applies

Each worker owns its sieve state in `worker_state`. For each pass, the worker:

- allocates a fresh dynamic sieve buffer
- initializes the buffer from scratch
- runs the sieve
- frees the buffer unless it is the final completed pass retained for validation

The extreme variant keeps the aggressive runtime-discovered marking optimizations inside `run_sieve`, but still recreates and initializes its sieve buffer for each pass.

### Why `bits=1` applies

Both variants store only odd candidates, using one packed bit per odd candidate.

## Optimized helper tables

The optimized jump tables and helper paths in `cwager_x64ff_mt_extreme.asm`:

- do not encode prior knowledge that any odd number is prime
- do not skip candidate discovery
- do not implement a wheel
- do not contain a hardcoded prime table
- include dense entries for every odd skip from `3` through `129`, including composites
- use residue-based sparse dispatch for larger skips

They are selected only after the candidate factor has already been discovered from the runtime sieve bitset.

## Build and run

From this directory:

    nasm -felf64 cwager_x64ff_mt.asm -o cwager_x64ff_mt.o
    gcc -no-pie -pthread cwager_x64ff_mt.o -o cwager_x64ff_mt

    nasm -felf64 cwager_x64ff_mt_extreme.asm -o cwager_x64ff_mt_extreme.o
    gcc -no-pie -pthread cwager_x64ff_mt_extreme.o -o cwager_x64ff_mt_extreme

The default helper scripts build and run both variants:

    ./build.sh
    ./run.sh

Run them individually:

    ./cwager_x64ff_mt
    ./cwager_x64ff_mt_extreme

## Docker

Build and run the containerized comparison:

    docker build -t cwager-x64ff-mt .
    docker run --rm cwager-x64ff-mt

## Output

Example recent outputs:

    cwager_x64ff_mt_extreme;380713;5.000;16;algorithm=base,faithful=yes,bits=1
    cwager_x64ff_mt;181358;5.000;16;algorithm=base,faithful=yes,bits=1

Exact pass counts vary by CPU, thread count, kernel scheduling, and system load.

## Notes

These implementations target amd64/x86-64 Linux and link against `pthread` and `libc`.
