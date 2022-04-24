# Implementation in R

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-16-yellowgreen)

This is an implementation of the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) algorithm in CMake. Note that this solution is not included in the automated benchmark runs. This is because the execution time of a sieve with limit 1,000,000 is prohibitively long.

## Bits per logical value

The CMake solution uses a list containing either 0/1 values. So 2 characters are used for each item: the value itself + its separator.
Assuming CMake uses utf-8 to encode strings, each item has a size of 16 bits.

### Special findings and performance tweaks

The CMake DSL is not really designed for mathematical operations + big array operations.

### Credits

@davepl for the original CPP submission that was used as a reference.

## Run instructions

### Run native

To run this solution you need "CMake". This program is available in most package managers of Linux distributions, or downloadable at https://cmake.org/download/. The script is designed to run in batch mode with `Rscript`.

```bash
cd path/to/sieve
cmake -P primes.cmake
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
Passes: 44, Time: 5.079399, Avg: 0.115440 (sec/pass), Limit: 1000, Count: 168, Valid: true

madebr_cmake;44;5.079399;1;algorithm=base,faithful=yes,bits=16
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-4702MQ CPU @ 2.2GHz, Fedora 34 64 bit
- CMake: 3.23.1
- running in Docker container `alpine:edge` (date=2022-04-22)
- Docker version 20.10.12, build 485636f
