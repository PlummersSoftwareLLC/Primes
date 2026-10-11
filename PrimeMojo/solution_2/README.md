# Mojo solution by lee101

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Single-threaded, faithful base sieve storing one bit per odd number in a
runtime-allocated `UInt64` buffer of `ceil((size + 1) / 2 / 64)` words. A set
bit marks a composite, so a zeroed buffer is the initial state.

The outer loop is the base one: scan odd numbers from 3 for the next unmarked
factor, then clear its multiples from `factor * factor` upward, one prime at a
time. Only the way each prime's multiples are written differs by prime size.

## Dense marking, factors up to 127

A factor's multiples repeat every `factor` words. That period is composed at
run time, after the factor has been found, by stepping the factor through it
and setting one bit per multiple. It is then replicated to a whole number of
SIMD vectors and ORed across the sieve in unrolled 64-word blocks, one vector
store per 4 words on AVX2 (8 on AVX-512). Each mask carries bits of one prime
only.

## Sparse marking, larger factors

Multiples are split into eight stripes by bit position within a byte, as in
the Rust, Nim and Lean entries. Inside a stripe the byte stride is `factor` and
the mask is constant. Because the start is `factor * factor`, the eight masks
depend only on `factor mod 16`, so Mojo `comptime` specialises the loop eight
ways and the masks become immediates. Every store clears one composite with a
single-bit mask.

## Sweep rotation

The 62.5 KB buffer is larger than a 32 KB L1D, so a plain ascending sweep per
prime evicts exactly the lines the next sweep needs first. Each sweep instead
starts 28 KB below where the previous one started and wraps around, so it
begins on the lines the previous sweep touched last. Every multiple is still
cleared individually; only the order within one prime's pass changes.

## Run instructions

```sh
docker build -t mojo-primes-2 .
docker run --rm mojo-primes-2
docker run --rm mojo-primes-2 --validate
```

`--validate` checks the count for every size from 0 to 29999 against a simple
reference sieve, and for 10 through 100,000,000 against known counts.

The Dockerfile pins Mojo `1.2.0.dev2026100805`; the source also builds with
Mojo 1.1.0.

## Output

Ryzen 9 5950X, pinned to one core, interleaved 5 s runs on a busy host:

| solution | passes |
|---|---|
| Mojo solution_2 (this) | 44186 - 44839 |
| Rust solution_1 `bit-extreme-hybrid` | 41266 - 43500 |
| Mojo solution_1 `ELucasCurrie_1bit` | 3956 |

```
lee101_1bit_dense_simd;44839;5.000090546;1;algorithm=base,faithful=yes,bits=1
```
