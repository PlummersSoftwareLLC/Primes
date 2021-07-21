# Object-Oriented Tcl implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)

This is an solution in object-oriented Tcl. Historically, Tcl did not have native support for object-oriented programming. As of version 8.6 however, Tcl includes a built-in object oriented system. Two implementation are created in this solution. The difference between the two is described below.

## oo_primes.tcl

This implementation uses two classes, `bit_array` and `prime_sieve`. The `bit_array` class implements a bit array with basic get and set methods. It uses a 32 bit string array as storage and each flag consumes one bit. The `prime_sieve` class is used for the actual sieve calculation.

This implementation is based on:

- Python/solution_2, by ssovest
- PrimeCPP,          by Dave Plummer
- [Bit vectors](https://wiki.tcl-lang.org/page/Bit+vectors), by Richard Suchenwirth

## oo_primes_2.tcl

The basic framework of this implementation is copied from [oo_primes.tcl](#oo_primes.tcl). However, in this implementation the `bit_array` variable is an array integers. The storage that is used for each flag is 32 bit. The size of the array is just the specified limit. Furthermore, `for` loops are replaced with `foreach` loops giving it a factor 2 performance improvement compared to [oo_primes.tcl](#oo_primes.tcl).

This implementation is based on:

- PrimeCPP,          by Dave Plummer

## Run instructions

### Run native

To run this solution you need Tcl. The Tcl shell (`tclsh`) is included in most Linux distributions.

```bash
cd path/to/sieve
./run.sh
```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t ootcl:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  ootcl:latest 
    ```

## Output

Below is an example of the output on my machine, running with Docker.

```bash
Passes: 3, Time: 5.256, Avg: 1.752 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

fvbakel_ootcl;3;5.256;1;algorithm=base,faithful=yes,bits=1
Passes: 9, Time: 5.025, Avg: 0.5583333333333333 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

fvbakel_ootcl2;9;5.025;1;algorithm=base,faithful=yes,bits=32
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- tcl: 8.6.11
- running in Docker container alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2
