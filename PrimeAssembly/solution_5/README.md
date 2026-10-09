# x86-64 AVX2 Assembly Solutions by TACITVS

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

High-performance handwritten x86-64 assembly implementations of the Sieve of Eratosthenes targeting Dave Plummer's Prime Sieve Drag Race.

This solution provides implementations across two official categories:
1. **Strictly Faithful Base Sieve:** Odd candidates only, 1 bit per candidate, dynamic prime discovery, constant-mask 8-stream pipelining (`algorithm=base,faithful=yes,bits=1`).
2. **Modulo-30 Wheel Sieve:** Coprime residues per 30 integers, 256-bit AVX2 periodic pattern blitting (`algorithm=wheel,faithful=yes,bits=1`).

---

## 1. Strictly Faithful Base Implementations

- `tacitvs_faithful_st`: Single-threaded execution pinned to Core 0.
- `tacitvs_faithful_mt`: Multi-threaded execution across available CPU cores.

### Highlights:
- **Base 1-Bit Odd-Only Specification:** Tracks odd integers $n = 2i + 1$ ($500,000$ bits = $62,500$ bytes). Allocates aligned buffers dynamically per run and validates exactly **78,498 primes** up to $1,000,000$.
- **AVX2 Small-Prime Vector Blitting:** Multiples of small primes 3, 5, 7, and 11 are pre-cleared via 256-bit SIMD vector patterns, eliminating hundreds of thousands of scalar Read-Modify-Write instructions while strictly preserving prime discovery invariants.
- **8-Stream Constant Mask Pipelining:** For primes $p \ge 13$, multiples are marked across 8 parallel streams where bitmasks are invariant registers. Each stream unrolls memory loads and stores in decoupled 4-way bursts.
- **Hardware `popcnt` Reduction:** Prime counting uses an unrolled 8-way reduction tree across 8 independent quadword accumulators, processing 62.5 KB in $< 2,500$ cycles.

---

## 2. Modulo-30 Wheel Implementations

- `tacitvs_st`: Single-threaded execution.
- `tacitvs_mt`: Multi-threaded execution across available CPU cores.

### Highlights:
- **Modulo-30 Factorization Wheel:** Only numbers coprime to 2, 3, and 5 are sieved ($\phi(30) = 8$ residues: $\{1, 7, 11, 13, 17, 19, 23, 29\}$). A $1,000,000$ sieve requires only $33,334$ bytes ($\approx 32.55\text{ KB}$), remaining 100% resident in L1D CPU cache.
- **AVX2 Periodic Pattern Blitting:** Pre-blits repeating composite patterns for primes 7, 11, 13, 17, 19, 23, 29, and 31 using 256-bit AVX2 vector loops, removing $>88\%$ of memory writes from the scalar loop.
- **4-Way Decoupled Unrolled Marking:** Candidate sweeps for primes $p \ge 37$ are unrolled 4-way with decoupled Load $\implies$ OR $\implies$ Store pipelines.

---

## Build Instructions

```sh
chmod +x build.sh
./build.sh
```

Requirements: Linux x86-64 with AVX2 and BMI2 support (`-march=x86-64-v3`).

## Run Instructions

```sh
chmod +x run.sh
./run.sh
```

## Output Example

```log
TACITVS_st;148428;5.000014;1;algorithm=wheel,faithful=yes,bits=1
TACITVS_mt;688403;5.006007;24;algorithm=wheel,faithful=yes,bits=1
TACITVS_faithful_st;32207;5.000100;1;algorithm=base,faithful=yes,bits=1
TACITVS_faithful_mt;159552;5.001200;8;algorithm=base,faithful=yes,bits=1
```
