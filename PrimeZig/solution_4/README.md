# Zig solution by joeshacks

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A single-threaded, faithful sieve for Zig 0.16. A `Sieve` struct owns a
freshly allocated bit buffer on every pass. That buffer stores odd numbers
only, and a set bit marks a composite.

Each factor's multiples are marked from `factor * factor` with a step of
`2 * factor`. How the marking loop runs depends on the step:

- **Steps below 64** (factors 3 to 61): every 64-bit word gets several marks,
  so whole words are OR-ed with the step's bit pattern. For an odd step `s`,
  the next word's mask is `(m >> d) | (m << (s - d))` with `d = 64 % s`.
  That lets the loop compute masks in registers, 16 words per `@Vector`
  operation, with no lookup table. The first factor writes into an all-zero
  buffer, so it stores its pattern instead of OR-ing it in.
- **Steps of 64 and up**: one bit per mark, through a byte view. Because the
  step is odd, eight steps advance exactly `step` bytes, so the bit pattern
  within a byte repeats every eight steps. A `switch` with `inline` cases
  creates 32 comptime variants, one per `(start % 8, step % 8)` pair. Each
  variant runs eight marks per iteration, using immediate masks and byte
  offsets of the form `k * (step / 8) + constant`.

The sieve size is read at runtime (with a default of 1,000,000), and only
the last sieve is counted and checked against the known prime count.

### Linux notes

- The image builds on Fedora 44, which packages Zig 0.16 and uses glibc. On
  Alpine, musl's `malloc` returns the 62.5 KB buffer to the kernel on every
  `free`, so each pass pays for fresh page faults. On the machine below,
  the same code made about 53,000 passes on Alpine and about 75,000 on
  Fedora.
- The buffer comes from `calloc`, not `alloc` + `@memset`. On Linux,
  `@memset` lowers to the `compiler_rt` memset linked into the binary, and
  that version writes one byte at a time.

## Run instructions

Docker:

```
docker build -t primezig-joeshacks .
docker run --rm primezig-joeshacks
```

Locally (Zig 0.16):

```
zig build-exe src/main.zig -O ReleaseFast -lc -femit-bin=PrimeZig
./PrimeZig            # 1,000,000
./PrimeZig 10000000   # other sizes up to 100,000,000 are validated too
```

## Output

On an Apple M1 Max, in Docker:

```
joeshacks;74959;5.000016;1;algorithm=base,faithful=yes,bits=1
```

For comparison, `PrimeCPP/solution_5` (`davepl_array_optimized`, single
thread) scored 70,383 to 71,574 under the same conditions.
