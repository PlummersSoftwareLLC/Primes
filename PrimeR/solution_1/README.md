# Implementation in R

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation of the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) algorithm in R. This implementation makes use of the reference class object oriented method that is available in R. This results in a solution that is closer to the rules defined in [CONTRIBUTION.md](../../CONTRIBUTION.md).

## Run instructions

### Run native

To run this solution you need "R". This program is available in most package managers of Linux distributions. The script is designed to run with `Rscript`.

```bash
cd path/to/sieve
Rscript primes.R
```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t r:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  r:latest 
    ```

Or do step 2 and 3 with `go.sh`.

## Output

Below is an example of the output on my machine, running with Docker.

```bash
docker run --rm -it  r:latest 
Passes: 1, Time: 0.985000, Avg: 0.985000 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

fvbakel_R;1;0.985000;1;algorithm=base,faithful=yes,bits=1
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- R: 4.0.3
- running in Docker container alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2
