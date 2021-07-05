# Zig solution by ManDeJan and ityonemo

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-16-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-64-yellowgreen)

Build:

```
zig build -Drelease-fast
```

Run (selected conditions):

```
./zig-out/bin/PrimeZig
```

Run (all permutations)

```
./zig-out/bin/PrimeZig --all
```

Uses highly composable Zig datastructures.
Features combinations of the following optimizations:

- datatypes: `bool`, `u1`, `u8`, `u16`...`u64`
- usage of bitmapping, sizes `u8`, `u16`, `u32`, `u64`
- multithreading:
  - Amdahl scaling: each thread works on a distinct 'job' which is running the sieve on a given factor.
    all threads coordinate towards a single solution.
  - Gustafson scaling:  each thread works independently and contributes independently to the common count.
- `wheel`:  Precalculate (at compile time) a series of initial primes and during the "reset" phase, project
     these primes onto the field.  Then proceed as normal with the remaining primes.

Architecture:

- `src/alloc.zig` an allocator that prioritizes use of scratch space set aside in .bss
- `src/main.zig` entrypoint for the program; builds the drag race layout
- `src/wheel.zig` uses the sieve program to generate wheel LUTs at compile-time
- `src/runners.zig` generic "runner" framework.  Impls:
  - `SingleThreadedRunner`
  - `AmdahlRunner`
  - `GustafsonRunner`
- `src/sieves.zig` generic sieves.  Impls:
  - `IntSieve` sieve stored as a (1/2) array of integers of comptime-defined sizes.
  - `BitSieve` sieve stored as a (1/2) array of bits, backed by comptime-defined unsigned integer types.
- `src/tests.zig` test cases.  Execute by running `zig test src/tests.zig`.  May not work correctly
  (tested on zig 0.7.2)