# Zig solution by ManDeJan and ityonemo and SpexGuy

## WARNING
The maintainers of the Primes program have instituted an limitation to the
competition rules, which requires that no more than an arbitrarily-decided 5s 
pause is permitted between runs.  On our test system we measured that a 5s pause
is suboptimal.  This puts the zig implementation at a disadvantage because it 
is modular and only costs one docker container per architecture, versus other 
language implementations which spread out different test conditions over multiple
containers (we have also measured that a docker build between runs is effective
at "cooling down" throttling).

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Build (curated conditions):

```
zig build -Drelease-fast
```

Build (broader set of conditions):

```
zig build -Drelease-fast -Dall
```

Note that this will consume a lot of memory.

Run:

```
./zig-out/bin/PrimeZig
```


Run just ONE (see `main.zig` for line numbers):

```
./zig-out/bin/PrimeZig -l <line number>
```

There is a special flag `-Darm-is-rpi` which adds extra filtering
on the units compiled (as detected by it being an arm architecture) that
allows you to compile on the memory-constrained rpi architecture.

Uses highly composable "Enterprise Zig" datastructures.
Features combinations of the following optimizations:

- int datatypes: `bool`, `u8`
- usage of bitmapping, sizes `u8`, `u16`, `u32`, `u64`
- manually unrolling the hot loops and making LUT functions.
- multithreading:
  - Amdahl scaling: each thread works on a distinct 'job' which is running the sieve on a given factor.
    all threads coordinate towards a single solution.
  - Gustafson scaling:  each thread works independently and contributes independently to the common count.
- `wheel`:  Precalculate (at compile time) a series of initial primes and during the "reset" phase, project
     these primes onto the field.  Then proceed as normal with the remaining primes.

and more!

Architecture:

- `src/alloc.zig` various allocators to be swapped out.
- `src/main.zig` entrypoint for the program; builds the drag race layout
- `src/wheel.zig` uses the sieve program to generate wheel memory LUTs at compile-time
- `src/runners.zig` generic "runner" framework.  Impls:
  - `SingleThreadedRunner`
  - `AmdahlRunner`
  - `GustafsonRunner`
- `src/sieves.zig` generic sieves.  Impls:
  - `IntSieve` sieve stored as a (1/2) array of integers of comptime-defined sizes.
  - `BitSieve` sieve stored as a (1/2) array of bits, backed by comptime-defined unsigned integer types.
- `src/unrolled.zig` implements compile-time unrolled functions.
- `src/tests.zig` test cases.  Execute by running `zig test src/tests.zig`.  May not work correctly
  (tested on zig 0.7.2)
