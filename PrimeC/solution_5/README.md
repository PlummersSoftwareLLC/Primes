# Implementation in C

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in C.
The algoritm is developed in NodeJS and C in parallel. 

## The memcopy algorithm


## Choice of Dockerfile

This solution uses Ubuntu 18.04 as the base image over alpine:3.13 because the `memcpy` function on alpine is slow due to the `musl libc`. The speed in this solution comes from the use of  `memcpy`, so it is extra slow on alpine.

## Run instructions

### Build and run native

To run this solution you need the gcc compiler and dependencies.

```bash
cd path/to/sieve
./compile.sh
./run.sh
```

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

Or you can do step 2 and 3 with `go.sh`.

## Output

Below is an example of the output on my machine, running with Docker.

```bash
```

