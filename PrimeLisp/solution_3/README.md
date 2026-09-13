# Common Lisp solution

A single-threaded sieve of Eratosthenes for **SBCL 2.6.8 on Linux x86-64
with AVX2**, using one bit per odd candidate. No additional Lisp libraries
are required. The implementation uses SBCL compiler internals and is not
portable to other Common Lisp implementations.

## How it works

`sieve.lisp` combines three optimizations:

- **Explicit memory lifetime.** Each pass creates a fresh sieve structure and
  zeroed native buffer. The structure has dynamic extent; SBCL's foreign-function
  interface calls `malloc`, `memset` and `free` for the buffer. `unwind-protect`
  guarantees release on normal and nonlocal exits. The measured loop needs no
  Lisp heap allocation or garbage collection.
- **Specialized marking kernels.** Lisp macros generate functions for small
  factors and for the repeating address patterns of larger factors. Local SBCL
  VOPs (compiler instruction-selection rules) emit AVX2 operations for dense
  marking and unrolled byte ORs for sparse marking. Dense masks are prepared at
  compile time and loaded outside the inner loop; bounded scalar tails handle
  incomplete vectors. This removes repeated mask construction and index work.
- **Cache locality.** The buffer is processed in 32 KiB blocks, limiting repeated
  traffic through the full array. Factors are discovered in the current sieve's
  first block. Later blocks preserve the marking patterns with a small bounded
  overlap. No prime list or completed sieve is cached between passes.

The output is tagged `algorithm=other,faithful=yes,bits=1`. Although the algorithm
is an Eratosthenes sieve, the dense kernels combine multiple composite flags in
one operation, unlike the project's `base` category. Each pass constructs its
entire state at runtime. C library calls only manage storage; the sieve, timing
and pass counting are implemented in Lisp and SBCL VOPs.

## Run

```sh
./run.sh                 # compile, validate, then benchmark for five seconds
./run.sh batch 5 3       # three five-second samples
./run.sh check           # independent full-flag and native-memory checks
./run.sh repl            # keep an SBCL image open for interactive work
```

Set `SBCL=/path/to/sbcl` if needed. Compilation and validation run before timing.
Benchmark attribution is read from `author.txt`, separately from the sieve code.
The benchmark includes fresh allocation, initialization, sieving and release on
each pass; validation and release of the final state follow the last timestamp.
The default limit is 1,000,000, checked against 78,498 primes. Extended checks
cover word and block boundaries, independent states, memory guards, and the
count of 664,579 primes at 10,000,000.

The Dockerfile bootstraps the pinned SBCL release from source using Debian's
packaged SBCL:

```sh
docker build -t primes-lisp-3 .
docker run --rm primes-lisp-3
```

## Origins and development

The dense/sparse marking strategy is inspired by Mike Barber and GordonBGood's
[`bit-unrolled-hybrid` Rust implementation](../../PrimeRust/solution_1).
Lisp macros provide the specialization performed there with Rust generics and
code generation. Rust disassembly informed the vector kernels; Callgrind cache
simulation helped guide the block traversal.

This implementation was developed by Codex, an AI coding agent, under human
direction. The agent worked interactively in a persistent SBCL image: redefining
and compiling functions, inspecting machine code with `disassemble`, and measuring
allocation, GC and execution time in the same REPL. Changes were saved in source
files, then reloaded and validated in fresh images. For example:

```lisp
(pm:with-sieve (s 1000000)
  (pm:run-sieve s)
  (pm:count-primes s)) ; => 78498
(disassemble 'pm::mark-block-by-17)
```

The state and its native address must not escape `with-sieve`.

## Example output

SBCL 2.6.8, Linux x86-64, Intel Xeon Platinum 8370C, one logical CPU:

```text
mrj-am-cl;49704;5.003980000;1;algorithm=other,faithful=yes,bits=1
```
