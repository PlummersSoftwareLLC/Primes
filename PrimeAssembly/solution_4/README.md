# cwager x86-64 NASM solutions

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This directory contains three multithreaded x86-64 NASM implementations of the base Sieve of Eratosthenes:

- `cwager_x64ff_mt.asm`
- `cwager_x64ff_mt_extreme.asm`
- `cwager_x64ff_mt_extreme_maskgen_onefactor.asm`

The helper scripts and Docker flow build and run all three variants for side-by-side comparison. `cwager_x64ff_mt.asm` is the conservative baseline, while `cwager_x64ff_mt_extreme.asm` and `cwager_x64ff_mt_extreme_maskgen_onefactor.asm` are aggressive performance-oriented variants.

All three implementations:

- sieve odd candidates only
- use a packed 1-bit representation for each odd candidate
- allocate and initialize a fresh sieve buffer dynamically for each timed pass
- discover every odd sieving factor from the runtime sieve state
- validate against the expected `78,498` primes at `1,000,000`
- print using the drag-race output format

## Variants

### `cwager_x64ff_mt.asm`

This is the conservative baseline variant.

Each worker thread:

- allocates a fresh sieve buffer dynamically at runtime for each pass
- initializes that buffer from scratch
- runs a faithful base-algorithm sieve over odd candidates only
- frees the sieve unless it is the final completed pass retained for validation

This version follows the repository wording for `faithful=yes` most directly.

### `cwager_x64ff_mt_extreme.asm`

This is the existing aggressive performance-oriented variant.

Key techniques:

- contiguous dense dispatch for every runtime-discovered odd skip from `3` through `129`
- dense handlers for composite odd skips as well as prime odd skips within that range
- dense handlers generated from source-level per-bit `btr` operations, rather than opaque precomputed bitmap constants
- sparse periodic marking helpers for larger runtime-discovered odd skips
- sparse dispatch based on modulo-16 residue classes
- per-pass dynamic sieve allocation with full reinitialization before each run

The optimized marking paths are selected only after the candidate factor has been found to still be set in the runtime sieve bitset.

The implementation does not embed:

- a wheel
- a prime table
- a hardcoded list of odd primes
- externally precomputed dense bitmap constants

### `cwager_x64ff_mt_extreme_maskgen_onefactor.asm`

This is an additional experimental aggressive variant.

Key techniques:

- runtime factor discovery from the sieve bitset before any dense or sparse helper is selected
- a contiguous odd-skip dense dispatch table covering `3` through `129`, including composite entries deliberately so the table is range-based rather than prime-list-based
- a reusable per-worker dense-mask scratch buffer allocated before timing starts
- blank scratch storage only, not a precomputed dense mask table
- one current-factor scratch mask built only after the dense-path factor has been discovered from the runtime sieve state
- scratch-mask composition in submitted NASM by filling with all ones and clearing individual multiple positions with visible `btr` operations
- timed dense-path application of that current-factor mask with `and`

The implementation does not use:

- a prime table
- a wheel table
- an externally generated mask file
- opaque precomputed bitmap constants
- a prebuilt range of dense masks

Its performance may vary by runner, so it is included as a separate comparison variant rather than as a replacement for `cwager_x64ff_mt_extreme.asm`.

## Rules / classification notes

Expected classification for all three variants:

- `algorithm=base`: yes
- `faithful=yes`: yes
- `bits=1`: yes

### Why `base` applies

All three variants scan odd candidate factors sequentially, starting at `3`.

Each candidate is tested against the runtime sieve state before any marking helper is selected. Once a candidate factor has been discovered from the sieve bitset, the code clears projected multiples of that factor from the sieve space.

The optimized dense and sparse helpers are projection optimizations after runtime discovery, not prior-prime knowledge.

The extreme variant's dense dispatch table covers the full contiguous odd skip range from `3` through `129`, including composite values such as `9`, `15`, `21`, and `25`.

Composite handlers are present in the table, even though they are normally not reached because those candidates have already been cleared by earlier factors.

The `cwager_x64ff_mt_extreme_maskgen_onefactor.asm` variant also keeps runtime factor discovery, but builds one current-factor scratch mask only after that factor has been discovered from the runtime sieve state.

### Dense mask / marking composition

For `cwager_x64ff_mt_extreme.asm`, the dense helpers are source-generated from NASM macros that emit identifiable individual `btr` operations for the relevant bit positions.

This means the submitted source shows the marking pattern being composed in code from individual multiple positions, rather than storing the finished marking pattern as opaque data such as a precomputed `dq` bitmap.

For `cwager_x64ff_mt_extreme_maskgen_onefactor.asm`, there is no prebuilt range of dense masks. Instead, after the currently discovered factor selects the dense path, the implementation builds one scratch mask for that current factor only.

That scratch mask is composed in submitted NASM by filling the scratch words with all ones and clearing the individual multiple positions with visible `btr` operations. The timed dense path then applies that current-factor mask with `and`.

Neither optimized variant uses a prime-only dispatch table, a prime table, wheel factorisation, external mask generation, or opaque precomputed dense bitmap constants.

### Why `faithful=yes` applies

Each worker owns its sieve state in `worker_state`.

For each pass, the worker:

- allocates a fresh dynamic sieve buffer
- initializes the buffer from scratch
- runs the sieve
- frees the buffer unless it is the final completed pass retained for validation

The aggressive variants keep their runtime-discovered marking optimizations inside `run_sieve`, but still recreate and initialize their sieve buffers for each pass.

### Why `bits=1` applies

All three variants store only odd candidates, using one packed bit per odd candidate.

## Optimized helper tables

The optimized jump tables and helper paths in `cwager_x64ff_mt_extreme.asm` and `cwager_x64ff_mt_extreme_maskgen_onefactor.asm`:

- do not encode prior knowledge that any odd number is prime
- do not skip candidate discovery
- do not implement a wheel
- do not contain a hardcoded prime table
- do not contain opaque precomputed dense bitmap constants
- include dense entries for every odd skip from `3` through `129`, including composites
- use residue-based sparse dispatch for larger skips
- are selected only after the candidate factor has already been discovered from the runtime sieve bitset
- in the onefactor variant, build one current-factor scratch mask after runtime factor discovery rather than using a prebuilt range of dense masks

The optimized helpers keep the relevant bit positions visible in the submitted source. The assembler may encode those instructions efficiently, but the source still identifies the individual bit positions being cleared.

## Build and run

From this directory:

```bash
nasm -felf64 cwager_x64ff_mt.asm -o cwager_x64ff_mt.o
gcc -no-pie -pthread cwager_x64ff_mt.o -o cwager_x64ff_mt

nasm -felf64 cwager_x64ff_mt_extreme.asm -o cwager_x64ff_mt_extreme.o
gcc -no-pie -pthread cwager_x64ff_mt_extreme.o -o cwager_x64ff_mt_extreme

nasm -felf64 cwager_x64ff_mt_extreme_maskgen_onefactor.asm -o cwager_x64ff_mt_extreme_maskgen_onefactor.o
gcc -no-pie -pthread cwager_x64ff_mt_extreme_maskgen_onefactor.o -o cwager_x64ff_mt_extreme_maskgen_onefactor
```

Then run any variant directly:

```bash
./cwager_x64ff_mt
./cwager_x64ff_mt_extreme
./cwager_x64ff_mt_extreme_maskgen_onefactor
```

Or use the provided helper scripts / Docker flow if preferred.

## Output

Example output:

```text
cwager_x64ff_mt_extreme;490663;5.000;12;algorithm=base,faithful=yes,bits=1
cwager_x64ff_mt_extreme_maskgen_onefactor;420890;5.000;12;algorithm=base,faithful=yes,bits=1
cwager_x64ff_mt;114516;5.000;12;algorithm=base,faithful=yes,bits=1
```

The exact pass counts will vary by CPU, operating system, container/runtime environment, thermal state, and thread scheduling, but the output format follows the Prime Sieve drag-race convention:

```text
implementation;passes;seconds;threads;algorithm=base,faithful=yes,bits=1
```
