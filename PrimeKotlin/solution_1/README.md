# Kotlin implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in [Kotlin](https://en.wikipedia.org/wiki/Kotlin_(programming_language)).

## Run instructions

### Run native

To run this solution you need Kotlin.

```bash
cd path/to/sieve
 primes.
```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t kotlin:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  kotlin:latest 
    ```

## Output

Below is an example of the output on my machine, running with Docker.

```bash

```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- Kotlin 1.5
- running in Docker container alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2
