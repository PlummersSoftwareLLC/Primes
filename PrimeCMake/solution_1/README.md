# Implementation in CMake

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This is an implementation of the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) algorithm in CMake. Note that this solution is not included in the automated benchmark runs. This is because the execution time of a sieve with limit 1,000,000 is prohibitively long.

## Bits per logical value

This solution uses CMake variables to store primeness, the value is "1" for primes and "0" for non-primes. 
The storage size is undetermined because the overhead for making this key access (fast) is unknown.
The minimum required size for each key/value is at least key size + 1.

### Special findings and performance tweaks

Early versions used lists and/or strings to store the sieve.
This incurred a lot of unnecessary copying + string look-up.
This can be avoided by using plain cmake variables.

### Credits

@davepl for the original CPP submission that was used as a reference.

## Run instructions

### Run native

To run this solution you need "CMake".
This program is available in most package managers of Linux distributions,
or downloadable at https://cmake.org/download/.
The script is designed to run in batch mode with `cmake -P`.

```bash
cd path/to/sieve
cmake -P primes.cmake
```

By default, the algorithm benchmarks the number of primes up to 1,000,000.
The number of primes for sieve sizes can be calculated by passing `-DSIEVE_SIZE=xxx`.
Make sure this argument goes *before* `-P primes.cmake`.

```bash
cd path/to/sieve
cmake -DSIEVE_SIZE=100  0 -P primes.cmake

```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f Dockerfile -t primes_cmake:latest .
    ```

3. Run with Docker:

    ```bash
    docker run --rm primes_cmake:latest
    ```

## Output

Below is an example of the output on my machine, running with Docker.

```bash
Passes: 1, Time: 20.169636, Avg: 20.169636 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

madebr_cmake;1;20.169636;1;algorithm=base,faithful=no
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-4702MQ CPU @ 2.2GHz, Fedora 34 64 bit
- CMake: 3.23.1
- running in Docker container `alpine:edge` (date=2022-04-22)
- Docker version 20.10.12, build 485636f
