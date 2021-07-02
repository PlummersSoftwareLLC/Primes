# Kotlin implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in [Kotlin-JS](https://en.wikipedia.org/wiki/Kotlin_(programming_language)). In this implementation javascript (Node.js) is used as the target platform. However the common modules of Kotlin are used as much as possible. Only the line `fun getSystemTimeInMillis() = kotlin.js.Date().getTime().toLong()` is specific to the javascript platform.

## Run instructions

### Run native

To build this solution you need Kotlin, minimal Java 1.8 and Node.js. Take the steps below to build and run this implementation.

```bash
cd path/to/sieve
mkdir ./node_modules/
unzip -j /opt/kotlinc/lib/kotlin-stdlib-js.jar "kotlin.js" -d ./node_module/
kotlinc-js -module-kind commonjs primes-js.kt -output primes.js
node primes.js
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
Passes: 408, Time: 5.001, Avg: 0.012257352941176471 (sec/pass), Limit: 1000000, Count: 78498, Valid: true

fvbakel_Kotlin_js;408;5.001;1;algorithm=base,faithful=yes,bits=1
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- Kotlin: 1.5.20
- Node.js: v16.3.0
- Running in Docker container Alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2
