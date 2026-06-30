# Koru solution by korulang

A Sieve of Eratosthenes in [Koru](https://korulang.org), a high-level
language whose compiler **generates** the specialized marker.

Koru: docs, examples, and design write-ups at [korulang.org](https://korulang.org) ·
source at [github.com/korulang/koru](https://github.com/korulang/koru).

> Koru is a young, pre-release language — this is an early showcase, and gentle
> feedback is genuinely welcome.

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## What this is

Koru is a high-level, effect-oriented language. The interesting thing it does for
this benchmark is in the marking step. The sieve calls:

```
std/field:mark-multiples(f, from, stride, limit)
```

`mark-multiples` is a `[transform]` — a **compile-time** event. The program states
*which* multiples to cross out; at each call site the compiler reads the access
pattern and **emits a specialized, fully-unrolled native marker**: one function per
stride and residue class, with the bit-masks baked in as compile-time immediates and
dispatched through a function-pointer table. It's the same machinery Koru uses to
compile a regular expression to a DFA: the readable, high-level call and the fast code
are the same source. You write "cross out the multiples"; the compiler writes the
specialized marker — a tight unrolled scalar loop that the backend (LLVM) is then free
to auto-vectorize. (To be precise: the Koru compiler generates the unrolled,
residue-specialized marker; it does not itself emit SIMD intrinsics — the
vectorization, where it happens, is the backend's.)

The **sieve, the 5-second timing loop, and the pass counting are all written in
Koru** (a `#L`/`@L` label-fold loop). The only thing that drops to a host effect is
reading the clock (`std/time:now`) — which is how Koru performs every effect, the
same category as a stdlib call in any language.

## Faithfulness

This entry is tagged `faithful=yes`. The case, stated plainly so reviewers can judge it:

- The sieve state is encapsulated in a `field` (Koru's bit-array type). Koru has no
  `class` keyword; an event-typed `field` is its closest equivalent — a value that
  carries the full sieve state. The source allocates a **fresh `field` every pass**
  (`std/field:new` … `std/field:free`) inside the timed loop — it is genuinely
  re-created from scratch each iteration, not a reused or static buffer.
- The buffer is sized to the sieve and allocated by the per-pass `new`.
- Koru's compiler applies **escape analysis**: it proves the per-pass `field` does not
  escape the loop body and places it on the stack. This is a compiler optimization of
  *where* the allocation lives — the program's meaning is unchanged — and it is the
  same optimization the JVM performs (escape analysis / scalar replacement) for Java
  entries already accepted as `faithful=yes` in this repository, and that Go performs
  for non-escaping `make()`.

One honest wrinkle, stated up front: the stack placement currently fires only when the
sieve size is a **compile-time constant** (here, `500000`). A strict reading of
"allocated dynamically at runtime" could question that — though the reference solutions
likewise hardcode the 1,000,000 constant, and the *allocation itself* happens per pass.
A runtime-sized variant would simply heap-allocate (and be faithful without argument),
at a small speed cost.

In other words: the source models faithful per-pass allocation; the compiler makes it
cheap underneath. If the maintainers read the rule differently, we're glad to discuss
or re-tag — the base algorithm, the fresh-instance-per-pass structure, and `bits=1`
are accurate regardless.

## Run instructions

The Dockerfile builds the Koru toolchain (`koruc`) **from source** at a pinned tag,
then compiles the sieve through Koru's full pipeline. Zig — `koruc`'s own build
toolchain — is fetched from the official ziglang.org release, the same way the
existing PrimeZig solutions obtain it.

```
docker build -t primekoru .
docker run --rm primekoru
```

Supports `amd64` and `arm64`.

## Output

The official result line is written to **stdout**:

```
korulang;<passes>;<seconds>;1;algorithm=base,faithful=yes,bits=1
```

A correctness check (π(1,000,000) = 78,498) is written to **stderr** as auxiliary
output:

```
validated primes: 78498
```
