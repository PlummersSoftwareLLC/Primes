# cwager Fortran solution

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithful](https://img.shields.io/badge/Faithful-yes-green)
![Parallel](https://img.shields.io/badge/Parallel-yes-green)
![Bits](https://img.shields.io/badge/Bits-1-green)

This solution is a faithful base Sieve of Eratosthenes written in Fortran with odd-only 1-bit storage and OpenMP workers. The sieve state is encapsulated in a derived type that owns the runtime-sized bitset buffer, and each benchmark pass creates a fresh sieve instance from scratch.

The implementation stays within `algorithm=base,faithful=yes,bits=1` by sequentially checking every odd factor from `3`, clearing composites one at a time from `factor * factor` with a `2 * factor` step, and avoiding wheel factorisation, precomputed prime tables, LUTs, strike masks, dense masks, and mask projection.

## Run instructions

Build locally with:

    ./build.sh

Run locally with:

    ./run.sh

Build the Docker image with:

    docker build -t primes-fortran-mt .

Run the Docker image with:

    docker run --rm primes-fortran-mt

You can constrain threads with `OMP_NUM_THREADS`, for example:

    OMP_NUM_THREADS=4 ./run.sh

## Output

Example output from a 16-thread run:

    cwager_fortran_mt;44546;5.004;16;algorithm=base,faithful=yes,bits=1

Exact pass counts vary by CPU, compiler, thread count, container runtime, and system load.