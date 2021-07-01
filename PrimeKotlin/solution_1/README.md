# Kotlin implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in [Kotlin](https://en.wikipedia.org/wiki/Kotlin_(programming_language)). In this implementation JVM (Java Virtual Machine) is used as the target platform. However the common modules of Kotlin are used as much as possible. Only the line `fun getSystemTimeInMillis() = System.currentTimeMillis()` is JVM specific. Changing this function to the platform specific function should be enough to build for example to native.

## Considerations

The Kotlin compiler can compile to Java byte code. This resulting byte code can be run in the Kotlin virtual machine or JVM. In this implementation the byte code is run in the JVM because that turns out to have a 10% performance improvement.

## Run instructions

### Run native

To run this solution you need Kotlin and minimal Java 1.8. Take the steps below to build and run this implementation.

```bash
cd path/to/sieve
kotlinc ./ -d primes.jar
java -jar primes.jar
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

Or do step 2 and 3 with `go.sh`.

## Output

Below is an example of the output on my machine, running with Docker.

```bash
Passes: 3718, Time: 5.002, Avg: 0.0013453469 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

fvbakel_Kotlin;3718;5.002;1;algorithm=base,faithful=yes,bits=1
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- Kotlin: 1.5.10
- Java: OpenJDK 64-Bit Server VM AdoptOpenJDK-11.0.11+9 (build 11.0.11+9, mixed mode)
- running in Docker container alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2
