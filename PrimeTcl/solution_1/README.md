# Tcl solution by fvbakel

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in Tcl, with a 32 bit string array as buffer
for the bit array. Native Tcl does not have objects, so this implementation uses a variable (`primes`) for the bit array. This variable is passed as a parameter to the `run_sieve` procedure to be as faithful as possible to the original and the rules. This implementation is based on the logic from:

- Python/solution_2, by ssovest
- PrimeCPP,          by Dave Plummer
- [Bit vectors](https://wiki.tcl-lang.org/page/Bit+vectors), by Richard Suchenwirth

## Run instructions

### Run native

To run this solution you need Tcl. The Tcl shell (`tclsh`) is included in most Linux distributions.

```bash
cd path/to/sieve
tclsh primes.tcl
```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t tclsolution1:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  tclsolution1:latest 
    ```

## Output

Below is an example of the output on my machine, running with Docker.

```bash
docker run --rm -it  tclsolution1:latest 
Passes: 4, Time: 6.137, Avg: 1.53425 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

fvbakeltcl;4;6.137;1;algorithm=base,faithful=yes,bits=1
```

These results are with the following conditions:
- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- tcl: 8.6.11
- running in Docker container alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2
