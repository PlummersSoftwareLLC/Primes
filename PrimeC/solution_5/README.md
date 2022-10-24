# Implementation in C

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in C.
The algoritm is developed in NodeJS and C in parallel. 

## The extend algorithm
The extend algoritm marks all the multiples of a prime factor in the range of the product of all previous primes and this prime x2.
So all multiples of 2,3 and 5 are marked until 2x3x5 = 30. In the range from 30-60 a reoccuring pattern emerges, 
which can be continued and repeated when extending the sieve. 

A number of techniques have been used to enable bit-level patterns to be extended fast. 
Also all possible optimizations have been used to speed up the code in C.

On initalization, a small benchmark searches for the right settings for the hardware and OS environment. 
This defaults to a really small and short tuning. Using the command line, this can be intensified.

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

