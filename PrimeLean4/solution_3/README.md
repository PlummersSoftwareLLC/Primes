# Lean 4 solution by ronald-d-rogers

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A Lean 4 implementation of the base sieve storing one **bit** per flag in a
`ByteArray`, single-threaded and faithful. The buffer is exactly
`⌈(1 + sieveSize / 2) / 8⌉` bytes — 62501 bytes for a sieve of 1,000,000.

## Why this is a separate solution

Flag storage differs from both earlier Lean entries, and it differs in a badge
characteristic: [`solution_1`](../solution_1) stores an `Array Bool` at one
machine word per flag, [`solution_2`](../solution_2) stores a `ByteArray` at one
byte per flag, and this stores one bit per flag.

Bit packing is not a free win in Lean. A straightforward 1-bit sieve, computing
`i / 8` and `i % 8` on every write, measured about 20% *slower* than the byte
version. What makes bit packing pay here is the marking order described below.

## Striped marking

The obvious way to set bit `i` is `bits[i / 8] |= 1 <<< (i % 8)`, which pays a
divide and a shift per write. The multiples of a base prime `p` can instead be
split into eight interleaved subsequences by `i % 8`. Within one subsequence the
bit position never changes — advancing eight steps of `p` in bit space advances
exactly `p` in byte space — so each subsequence is a loop whose mask and byte
stride are both **loop-invariant**, computed once from the discovered prime:

```
for b := start/8, start/8 + p, start/8 + 2p, …    bits[b] |= mask
```

No bit arithmetic survives in the hot loop. This is the same transformation the
Rust, Nim, Java and F# entries use, and like them it remains `algorithm=base`:
each store clears one composite, and only the order changes.

Against solution_2 — same structure, same harness, byte per flag — this is about
1.38x. Three effects combine: the buffer drops from 500001 to 62501 bytes and so
fits in L1 (128 KB on the test machine, which the byte buffer never did), the
zero-fill shrinks eightfold, and the per-write index arithmetic disappears.

## Dense marking for the smallest primes

Striping issues one store per composite. For `p = 3` and `p = 5` a single byte
holds two or three multiples of `p`, so those stores repeatedly read, modify and
write the same byte. Marking such a prime densely instead — walking every byte
once and OR-ing a mask carrying all of that prime's bits for that byte — costs
one store per byte regardless of `p`.

Which bits a byte needs depends only on its index modulo `p`, because a byte
advances the bit index by 8. So one period of `p` masks is composed per prime,
after that prime has been discovered from the sieve, by stepping it eight times
and setting one bit per step with `1 <<< (i % 8)`. The byte loop is then
unrolled `p` times so each mask reaches its store as a loop-invariant runtime
value rather than something re-derived per byte.

Measured cost of clearing every multiple of 3 across the 62501-byte buffer,
20000 sweeps, on an Apple M4 Max:

| strategy | stores | time per sweep |
|---|---|---|
| striped bytes | 166669 | 47.7 µs |
| dense bytes | 62501 | 21.8 µs |
| dense 64-bit words | 7813 | 10.1 µs |

Dense stores are slightly dearer each (0.35 ns against 0.29 ns), so dense only
pays while a prime averages more than one multiple per byte, which puts
break-even just under `p = 7`. Hence 3 and 5 are marked densely and 7 upward are
striped, worth 1.16x over striping alone.

This is still `algorithm=base`, and the reason is that every bit of every mask
is an identifiable marking operation in the source. `periodMasks` composes a
mask by stepping the discovered prime through one period and setting one bit per
step with `1 <<< (i % 8)`; nothing arrives as finished bitmap data, and no mask
is built before the prime it belongs to has been found in the sieve. The masks
then reach `markDense3` and `markDense5` as ordinary runtime arguments.

The two halves of the dense path are worth separating, because it is the first
that earns the label. Composing the mask steps the prime through one period,
one multiple at a time. Applying it then walks the buffer once per byte, so a
single store can clear several of that prime's multiples at once. Each mask
carries bits for one prime only.

The precedents for this shape are PrimeV/solution_2, whose `dense_bitset`
composes per-bit with single-bit literals, and PrimeAssembly/solution_4's
`cwager_x64ff_mt_extreme_maskgen_onefactor`, which builds one scratch mask per
runtime-discovered factor with visible per-multiple bit operations and then
applies it. Both report `algorithm=base`. The second is the closer match, since
it composes a scratch mask and then applies it rather than fusing the two.

As a related technique rather than a precedent, the Nim entry's `denseSetBits`
and the recently added dense small-factor path in PrimeSwift/solution_1 do the
same work in fused form, OR-ing each multiple directly into a register holding
the sieve word before committing it. Their thresholds follow from word
granularity, not from anything about classification.

## Why not 64-bit words

The word column above is the obvious next step and is what the fastest entries
do, but Lean has no packed word array to mark through. `ByteArray` is
byte-addressed, and `Array UInt64` holds one object reference per element with a
heap-allocated box behind each, since a 64-bit value cannot be a tagged
immediate.

`FloatArray` is packed at eight bytes per element and can be pressed into
service as a word buffer via `Float.toBits`/`Float.ofBits`, which is how the
word row was measured. It loses overall anyway: `lean_float_to_bits` is an
out-of-line export in `libleanrt` rather than a `static inline`, so every word
access pays two real calls, putting it at 1.29 ns per store against 0.29 ns for
a byte store. That is affordable when one store covers 64 flags, but the ~385000
sparse stores from primes 17 and up would each cost 4.5x more. On this sieve the
word buffer would save roughly 70 µs on the small primes and lose roughly 250 µs
on the rest.

## Implementation notes

Storage is odd-only: bit `k` represents the number `2k+1`, so stepping the bit
index by `factor` steps the number by `2 * factor`. Marking is inverted, with
`0` meaning "still a prime candidate", so a freshly zeroed buffer is already the
correct initial state.

Three techniques carry over from solution_2, where they are documented at
length, and are Lean-specific enough to restate:

**The hot loops are tail-recursive functions, not `while` loops over mutable
state.** A `USize` held as a mutable binding is heap-allocated every iteration —
the generated C shows a `lean_box_usize` call in the loop body — because a
`USize` cannot be a tagged immediate the way a small `Nat` can. That is a malloc
per marked composite. As function parameters these are unboxed `size_t`.

**Bounds checks are discharged by proof, not disabled.** The loops carry an
erased proof that the limit is within the buffer and derive each access's
obligation from the loop comparison already made, so `uget`/`uset` compile to
plain indexed accesses. The safe `get!`/`set!` would repeat that comparison at
run time. This moves the check to compile time rather than skipping it; Lean's
equivalent of `-no-bounds-check` is not used.

The dense loops need one extra piece to make this work. `partial def` loops are
opaque about their results, so a size invariant cannot be threaded through one.
The single guarded OR is therefore factored out as a non-recursive `orAt`, whose
size behaviour *is* provable, and the unrolled loop rewrites the invariant
across each store rather than re-deriving it from a run-time check.

**The zero-fill is hand-rolled because Lean has no native one.** `ByteArray` has
no zero-fill primitive, and the obvious `⟨Array.replicate n 0⟩` first builds a
boxed `Array UInt8` at one machine word per element. `growExact` doubles an owned
`ByteArray` instead, staying packed, and does one final partial append so the
result is exactly the requested size rather than rounded up to a power of two.

A measurement worth recording for anyone optimizing this further: with the
marking loops neutered so that only allocation, the outer prime scan and the
per-stripe setup remain, a pass costs under 7 µs of the 221 µs it takes in full.
Stores are ~97% of the run, so store count is the only lever that matters.

## Run instructions

Install the Lean version manager [elan](https://github.com/leanprover/elan).
Then in this directory:

```sh
lake build
./.lake/build/bin/PrimeLean4
```

`lake` reads `lean-toolchain` and fetches the pinned Lean version automatically.

Passing `--validate` prints counts and buffer sizes for sieve sizes from 10 to
10,000,000 instead of running the benchmark.

### Docker

```sh
docker build -t primes-lean4-3 .
docker run --rm primes-lean4-3
```

The Dockerfile uses a build stage for the toolchain and copies only the linked
binary into the runtime image.

## Output

The prime count is validated against the expected 78498 on stderr before the
timed run. The buffer size is reported there too, so the exact-sizing claim can
be checked rather than taken on trust.

On an Apple M4 Max, with the Lean version pinned in `lean-toolchain`:

```
validate: sieveSize=1000000 bufferBytes=62501 count=78498 expected=78498 PASS
ronald-d-rogers_lean4_striped1;22616;5.000189;1;algorithm=base,faithful=yes,bits=1
```

Best of three, interleaved against solution_2 on the same machine and in the
same session: 22616 passes here against 14123 for solution_2, or about 1.60x.

Measured during review on native x86-64, also interleaved best-of-three: 24251
against 14156, or about 1.71x. Those are not my numbers and are attributed
rather than claimed. All figures come from single machines and say nothing about
how anything places on the benchmark server.
