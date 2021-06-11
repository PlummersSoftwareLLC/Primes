# Zig solution by ManDeJan and ityonemo

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-<wheel>-yellowgreen)

Build:

```
zig build -Drelease-fast
```

Run:

```
./zig-out/bin/PrimeZig
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
- `src/pregen.zig` uses the sieve program to generate wheel LUTs at compile-time
- `src/runners.zig` generic "runner" framework.  Impls:
  - `SingleThreadedRunner`
  - `AmdahlRunner`
  - `GustafsonRunner`
- `src/sieves.zig` generic sieves.  Impls:
  - `IntSieve` sieve stored as a (1/2) array of integers of comptime-defined sizes.
  - `BitSieve` sieve stored as a (1/2) array of bits, backed by comptime-defined unsigned integer types.
- `src/tests.zig` test cases.  Execute by running `zig test src/tests.zig`.

Notes:

As is program is currently not working on zig 0.8.0 (`zig build`) the build did not survive transitioning
from zig 0.7.1 due to the system recognizing more undefined behaviour, but it works correctly in unsafe
zig; will be looking into fixing this.