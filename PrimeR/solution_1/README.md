# Implementation in R

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)

This is an implementation of the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) algorithm in R. This implementation makes use of the reference class object oriented method that is available in R. This results in a solution that is closer to the rules defined in [CONTRIBUTING.md](../../CONTRIBUTING.md).

## Bits per logical value

The number of bits that are used per logical value in R is difficult to determine. To store a single logical value R uses 48 bytes. Storing 4 logical values uses 64 bytes and 5 logical values use 80 bytes. This strange behavior is due to the way R handles memory usage for small vectors, and the fact that R allows 'NA' values for the logical type too. However, as the number of logical values grow, the storage becomes more efficient. The method below was used to determine the number of bits that are used per logical value in the case we have 1 million entries.

```R
> b_array <-rep(TRUE,1000000)
> object.size(b_array)
4000048 bytes
```

It turns out that 32 bits per logical value are used when a vector has the size of one million logical values. More information on the memory management by R is described [here](http://adv-r.had.co.nz/memory.html).

### Special findings and performance tweaks

It turns out that finding the next prime in R is really slow if you use, `which`, `Position` or `match`. Even a second while loop is much faster, but still slow. In the latest code this is solved by adding `if (bit_array[factor] == TRUE) {`. This implements the search for the next prime in the main while loop with out the need of an inner loop. Somehow this makes a difference in R.

### Credits

Special thanks to @nobrien97 for pointing out the possible code improvement in my original code an better Dockerfile.

## Run instructions

### Run native

To run this solution you need "R". This program is available in most package managers of Linux distributions. The script is designed to run in batch mode with `Rscript`.

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
Passes: 463, Time: 5.001000, Avg: 0.010801 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

fvbakel_R;463;5.001000;1;algorithm=base,faithful=yes,bits=32
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- R: 4.1.0
- running in Docker container alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2
