# Lean 4 solution by ronald-d-rogers

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

A Lean 4 implementation of the base sieve storing one byte per flag in a
`ByteArray`, single-threaded and faithful.

## Why this is a separate solution

The substantive difference from [`solution_1`](../solution_1) is the flag
storage type. solution_1 stores flags in an `Array Bool`; this solution stores
them in a `ByteArray`. Those are different data types with different memory
layouts, and the distinction shows up in a badge characteristic rather than only
internally.

From `include/lean/lean.h` in the pinned toolchain, an `Array` holds one object
reference per element:

```c
typedef struct {
    lean_object   m_header;
    size_t        m_size;
    size_t        m_capacity;
    lean_object * m_data[];
} lean_array_object;
```

Nothing is allocated behind those references for a `Bool`, because scalars are
tagged immediates encoded in the word itself — `lean_box` is
`(lean_object*)((n << 1) | 1)`. So a `Bool` slot holds the literal word, and the
slot is the whole per-flag cost: one machine word, which is `sizeof(lean_object*)`
and therefore target-dependent.

A `ByteArray` is a different structure with a fixed element width:

```c
typedef struct {
    lean_object   m_header;
    size_t        m_size;
    size_t        m_capacity;
    uint8_t       m_data[];
} lean_sarray_object;
```

That is one byte per flag on any target, so `bits=8` here is both correct and
portable in a way it would not be for an `Array Bool`.

This solution also builds with `lake` on `leanprover/lean4:v4.33.0` rather than
`leanpkg` on the 2021 `4.0.0-m2` toolchain. That part is incidental — it is
needed simply because time has passed, and it is not a reason for a separate
entry on its own.

## Implementation notes

Storage is odd-only: index `k` represents the number `2k+1`, the buffer is
exactly `1 + sieveSize / 2` bytes, and stepping the index by `factor` is the same
as stepping the number by `2 * factor`.

Four things in the source are Lean-specific and worth calling out, since they
look unusual next to the C and Rust solutions.

**The hot loops are tail-recursive functions, not `while` loops over mutable
state.** This is the single largest factor, and the reason is counterintuitive
enough to be worth stating plainly: a `USize` held as a mutable binding gets
heap-allocated on every iteration. The generated C shows it directly, as a
`lean_box_usize` call inside the loop body, which is a malloc per marked
composite. A `USize` cannot be a tagged immediate the way a small `Nat` can, so
the "obviously faster" unboxed integer type becomes the slower choice in that
position. Measured during review of this solution, rewriting these loops as
`while` loops over mutable `USize` state costs 9.6x — slower than solution_1 —
and `Nat` with `while` loops beats `USize` with `while` loops by 4.5x. Passed as
function parameters, as they are here, `USize` values are unboxed `size_t`.

**Bounds checks are discharged by proof, not disabled.** The hot loops carry an
erased proof that `len ≤ bits.size` and derive each access's bounds obligation
from the loop comparison that has already been made. `uget`/`uset` then compile
to plain indexed loads and stores. The safe `get!`/`set!` operations would repeat
the same comparison at run time; this moves it to compile time rather than
skipping it. Lean's equivalent of `-no-bounds-check` is not used. Worth 1.24x.

**The zero-fill is hand-rolled because Lean has no native one.** `ByteArray` has
no zero-fill primitive, and the obvious `⟨Array.replicate n 0⟩` first builds a
boxed `Array UInt8` at one word per element — a 4 MB intermediate for a 500 KB
buffer — before packing it down. `growExact` instead doubles an owned `ByteArray`,
staying in packed representation, and does one final partial append so the result
is the exact requested size rather than rounded up to a power of two. Worth 1.82x,
which matters because the rules require the allocation to happen once per pass.

**The timing loop reads the sieve size back through a ref.** `Sieve.create sz` is
pure and loop-invariant, so the compiler may otherwise hoist it out of the timing
loop and run the sieve once. The ref read is the same guard the C solutions get
from `volatile` and the Rust ones from `std::hint::black_box`: one pointer load
per pass, and it does not touch the sieve.

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

The Dockerfile uses a build stage for the toolchain and copies only the linked
binary into the runtime image, which needs nothing beyond base libc.

## Output

The prime count is validated against the expected 78498 on stderr before the
timed run. The buffer size is reported there too, so the exact-sizing claim can
be checked rather than taken on trust.

On an Apple M4 Max, with the Lean version pinned in `lean-toolchain`:

```
validate: sieveSize=1000000 bufferBytes=500001 count=78498 expected=78498 PASS
ronald-d-rogers_lean4_bool8;14154;5.000092;1;algorithm=base,faithful=yes,bits=8
```

Against solution_1, measured during review on native x86-64, best of five runs:
2937 passes per five seconds for solution_1 against 13769 for this solution, or
about 4.7x.

For further reference, single-threaded base sieves with 8-bit flags written
directly in C and Rust — same algorithm, same odd-only byte buffer, same
5-second harness — reach 3051 and 3009 passes/second natively on the M4 Max,
against 2831 for this solution. Those are my own reference implementations
rather than any of the solutions in this repository, and all numbers are
best-of-three on one machine; they say nothing about how anything places on the
benchmark server.
