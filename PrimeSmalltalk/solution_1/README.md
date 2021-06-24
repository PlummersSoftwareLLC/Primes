
# Smalltalk implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in [GNU Smalltalk](https://www.gnu.org/software/smalltalk/). In this language there is no equivalent class for `BitArray` available. So this implementation has a custom `BitArray` class, that uses the `ByteArray` class as storage. It was also tested if there is performance difference with using the `WordArray` as storage class, but that seems to have no impact.

## Run instructions

### Run native

To run this solution you need GNU Smalltalk. This is available in the package managers of most in most Linux distributions as `gnu-smalltalk`.

```bash
cd path/to/sieve
gst -g primes.st
```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t smalltalk:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  smalltalk:latest 
    ```

Or do step 2 and 3 with `go.sh`.

## Output

Below is an example of the output on my machine, running with Docker.

```bash
Passes: 20, Time: 5.247, Avg: 0.26235 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

fvbakel_smalltalk;20;5.247;1;algorithm=base,faithful=yes,bits=1
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- GNU Smalltalk: 3.2.5
- running in Docker container ubuntu:18.04
- Docker version 20.10.2, build 20.10.2-0ubuntu2
