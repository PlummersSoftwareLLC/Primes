# Lean 4 solution by ronald-d-rogers

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

A performance-oriented Lean 4 implementation of the base sieve with 8-bit flags.

[`solution_1`](../solution_1) was written against `leanprover/lean4:4.0.0-m2` and
notes that "not much has been done to optimize this implementation"; it also
builds with `leanpkg`, which no longer ships with Lean. This solution targets a
current toolchain (`leanprover/lean4:v4.33.0`, built with `lake`) and encodes the
same algorithm in a way that avoids the boxing and allocation costs that dominate
a direct transcription. The algorithm itself is unchanged.

## Implementation notes

Storage is odd-only: one byte per odd candidate, so index `k` represents the
number `2k+1` and the buffer is exactly `1 + sieveSize / 2` bytes. Stepping the
index by `factor` is therefore the same as stepping the number by `2 * factor`.

Three things in the source are Lean-specific and worth calling out, since they
look unusual next to the C and Rust solutions:

**Bounds checks are discharged by proof, not disabled.** The hot loops carry an
erased proof that `len ≤ bits.size` and derive each access's bounds obligation
from the loop comparison that has already been made. `uget`/`uset` then compile
to plain indexed loads and stores. The safe `get!`/`set!` operations would repeat
the same comparison at run time; this moves it to compile time rather than
skipping it. Lean's equivalent of `-no-bounds-check` is not used.

**The zero-fill is hand-rolled because Lean has no native one.** `ByteArray` has
no zero-fill primitive, and the obvious `⟨Array.replicate n 0⟩` first builds a
boxed `Array UInt8` at eight bytes per element — a 4 MB intermediate for a 500 KB
buffer — before packing it down. `growExact` instead doubles an owned `ByteArray`,
staying in packed representation, and does one final partial append so the result
is the exact requested size rather than rounded up to a power of two. This was
the single largest cost in the buffer allocation, which the rules require to
happen once per pass.

**The timing loop reads the sieve size back through a ref.** `Sieve.create sz` is
pure and loop-invariant, so the compiler may otherwise hoist it out of the timing
loop and run the sieve once. The ref read is the same guard the C solutions get
from `volatile` and the Rust ones from `std::hint::black_box`: one pointer load
per pass, and it does not touch the sieve.

The loops are tail-recursive functions rather than `while` loops with mutable
bindings, because a `USize` carried as mutable state gets heap-boxed while a
`USize` function parameter is passed unboxed as a `size_t`.

## Run instructions

Install the Lean version manager [elan](https://github.com/leanprover/elan). Then
in this directory:

```sh
lake build
./.lake/build/bin/PrimeLean4
```

`lake` reads `lean-toolchain` and fetches the pinned Lean version automatically.

### Docker

```sh
docker build -t primes-lean4-2 .
docker run --rm primes-lean4-2
```

## Output

The prime count is validated against the expected 78498 on stderr before the
timed run. The buffer size is reported there too, so the exact-sizing claim can
be checked rather than taken on trust.

On an Apple M4 Max, with the Lean version pinned in `lean-toolchain`:

```
validate: sieveSize=1000000 bufferBytes=500001 count=78498 expected=78498 PASS
ronald-d-rogers_lean4_bool8;14154;5.000092;1;algorithm=base,faithful=yes,bits=8
```

Built and run as containers on the same host, this solution completes 2484 passes
where [`solution_1`](../solution_1) completes 348 — about 7.1x. Both containers
were built and run identically, so this is a like-for-like comparison of the two
Lean encodings; the absolute numbers are low because the containers run under
x86-64 emulation on an ARM host.

For further reference, single-threaded base sieves with 8-bit flags written
directly in C and Rust — same algorithm, same odd-only byte buffer, same
5-second harness — reach 3051 and 3009 passes/second natively on this machine,
against 2831 for this solution. Those are my own reference implementations
rather than any of the solutions in this repository, and all numbers are
best-of-three on one machine; they say nothing about how anything places on the
benchmark server.
