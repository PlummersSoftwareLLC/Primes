# Zig solution by ManDeJan and ityonemo

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-<wheel>-yellowgreen)

Build:

```
zig build -Drelease-fast
```

Run:

```
./zig-cache/bin/PrimeZig
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