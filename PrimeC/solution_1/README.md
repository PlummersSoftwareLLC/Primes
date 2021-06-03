# C solution by mckoss

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Single threaded implementation in C of prime number sieve.

Several algorithm optimizations are applied to achieve a fast running time.

- Not just ignoring even bits in the sieve bitmap, but also skipping higher
  multiples of 3 and 5 as well.
- Precomputing bit-masks for re-use in marking passes.

## Run instructions

Compile with GCC optimizing for fast execution and then running the
compiled program by executing this script:

> $ ./run

Alternatively, you can use docker to run this code from a container.

```
$ docker build -t drag-race .
$ docker run drag-race
```

or as a one-liner:

```
$ docker run --rm $(docker build -q .)
mckoss-c830;14196;5.0;1
```

*For some reason - I get 1/2 the performance running `sieve` on the same hardware by running this
in a Docker container as I do running natively in Windows or even in Linux running in a VM
on Windows.*

## Output

```
$ ./run
mckoss-c830;25155;5.0;1
```
Or, more verbosely:

```
$ ./run --debug
model name      : Intel(R) Core(TM) i7-8700K CPU @ 3.70GHz
cpu MHz         : 3700.000
cache size      : 12288 KB
Calculate primes up to 1000000.
Timer resolution: 1000 ticks per second.
Word size: 64 bits.

2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997, ...
Found 78498 primes up to 1000000.

mckoss-c830;25186;5.0;1;algorithm=wheel,faithful=yes,bits=1
```
