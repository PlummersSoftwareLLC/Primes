# Kotlin implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This is an implementation in [Kotlin](https://en.wikipedia.org/wiki/Kotlin_(programming_language)). In this implementation Linux Native is used as the target platform. The source file from [solution_1/primes.kt](../solution_1/primes.kt) is used as a basis and only the platform specific lines are changed.

Only the line `fun getSystemTimeInMillis() = kotlin.system.getTimeMillis()` is specific to the native platform. This line and the output lines are the only difference with [solution_1/primes.kt](../solution_1/primes.kt).

## Considerations

The Kotlin native library allows the class `BitSet`. However it turns out that using this class is extremely slow.

## Run instructions

### Run native

To build this solution you need Kotlin-native and minimal Java 1.8. Take the steps below to build and run this implementation.

```bash
cd path/to/sieve
kotlinc-native primes-native.kt -opt -o primes
./primes.kexe
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
Passes: 1245, Time: 5.004, Avg: 0.004019277 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

fvbakel_Kotlin_native;1245;5.004;1;algorithm=base,faithful=yes,bits=1
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- Kotlin: 1.5.20
- Running in Docker container Ubuntu:18.04
- Docker version 20.10.2, build 20.10.2-0ubuntu2
