# Implementation in C

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in C.
The algorithm is developed in NodeJS and C in parallel. 

## The extend algorithm
The extend algorithm marks all the multiples of a prime factor in the range of the product of the prime and all previous primes x2. E.g.: all multiples of 2,3 and 5 are marked until 2x (1x2x3x5) = 30. The range from 15-30 is a reoccuring pattern. So when we find 7, we can extend the pattern 15-30 until 7x15 = 105. Then we can mark all multiples of 7, and so on. So by gradually extending the seive by repeating the current pattern, we can have significant efficiency gains. 

For larger primes, the range will be so large that it can't be effiently handles by the L1 cache. Therefore, the sieve is divided in blocks, so that the multiples are handler per group. The blocks can be entirely independent (start at prime x and then use the extend algortim again), but a hybrid approach is faster: keep extending till the range for the first product of primes extends the sieve. Then, stripe of per block. 

A number of techniques have been used to enable bit-level patterns to be extended fast. 
Also all possible optimizations have been used to speed up the code in C.
On initalization, a small benchmark searches for the right settings for the hardware and OS environment. 
This defaults to a really small and short tuning. Using the command line, this can be intensified.

Inspired by: 
- nodeJS/solution_1 - rogiervandam-memcopy. This is the implementation in C, to see how much speed can be gained by moving from nodeJS to C. 
- PrimeC/solution_3 - fvbakel C-words. The segmented algorithm has similar concepts. But the extended algorithm implementations takes it further, by speed gains with a sub-byte (bit) level algorithms (race, pattern, small vs largestep optimizations).

## Choice of Dockerfile
This solution uses Ubuntu 20.04 as the base image.
The gcc image at gcc:12-bullseye seems to be slower.
ALso the alpine:3.13 base image has a slow `memcpy` function due to the `musl libc`. 

## Run instructions

### Build and run native

To run this solution you need the gcc compiler and dependencies.

```bash
cd path/to/sieve
./compile.sh
./run.sh
```

or use the shortcut ./test.sh sieve_extend

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t c:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  c:latest 
    ```

## Output

Below is an example of the output on my machine, running with Docker.

```bash
rogiervandam_extend;46042;5.000072;1;algorithm=other,faithful=yes,bits=1 
```
