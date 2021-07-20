# Scheme solution by William103

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)

`sieve.ss` creates a library encapsulating the logic and state of the prime
sieve, as well as implements a bit vector using scheme's `byte-vector`.

`PrimeScheme.ss` uses that library to compute the primes less than 1,000,000 as
many times as possible in five seconds.

`run.ss` compiles and runs the entire program

## Run instructions

If you want to run it manually, simply install [Chez
Scheme](https://cisco.github.io/ChezScheme/) and run `./run.ss` and everything
should just work.

Alternatively, you can use `docker` to build and run the provided dockerfile:

    $ cd PrimeScheme/solution_1
    $ docker build -t scheme1 .
    $ docker run --rm scheme1

## Output

Using chezscheme 9.5.4 on Arch Linux, I get

    compiling sieve.ss with output to sieve.so
    compiling PrimeScheme.ss with output to PrimeScheme.so
    William103;134;5.030902191;1;algorithm=base,bits=1,faithful=yes
