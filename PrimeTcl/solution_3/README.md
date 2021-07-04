# Object-Oriented Tcl implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)

This is an implementation in object-oriented Tcl. Historically, Tcl did not have native support for object-oriented programming. As of version 8.6 however, Tcl includes a built-in object oriented system.

The basic framework of this solution is copied from [solution_2](../solution_2). However, in this implementation the `bit_array` variable is just an array of 32 bit integers. Further more, `for` loops are replaced with `foreach` loops giving it a factor 2 performance improvement compared to [solution_2](../solution_2).

This implementation is based on:

- PrimeCPP,          by Dave Plummer

## Run instructions

### Run native

To run this solution you need Tcl. The Tcl shell (`tclsh`) is included in most Linux distributions.

```bash
cd path/to/sieve
tclsh oo_primes.tcl
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
Passes: 10, Time: 5.265, Avg: 0.5265 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

fvbakel_ootcl2;10;5.265;1;algorithm=base,faithful=no,bits=32
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- tcl: 8.6.11
- running in Docker container alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2
