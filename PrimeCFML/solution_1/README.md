# CFML (Lucee) solution by willeyeuk

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-64-yellowgreen)

Single-threaded & Multi-threaded solution writen for Lucee 7 (CFML). It includes implementations using Numbers, Booleans and a Java BitSet.#

Multi-threaded solution defaults to match CPU cores but can be overridden with -e MAX_THREADS=n

## Build and run instructions

```shell
docker build -t primes-cfml1 .
docker run --rm primes-cfml1
```

## Output

```shell
Passes: 24, Time: 5.153, Avg: 0.214708333333, Limit: 1000000, Count: 78498, Valid: true
willeyeuk-numbers;24;5.153;1;algorithm=base,faithful=yes,bits=64

Passes: 25, Time: 5.074, Avg: 0.20296, Limit: 1000000, Count: 78498, Valid: true
willeyeuk-bitset;25;5.074;1;algorithm=base,faithful=yes,bits=1

Passes: 43, Time: 5.042, Avg: 0.117255813953, Limit: 1000000, Count: 78498, Valid: true
willeyeuk-booleans;43;5.042;1;algorithm=base,faithful=yes,bits=64

Passes: 125, Time: 5.287, Avg: 0.042296, Limit: 1000000, Count: 78498, Valid: true
willeyeuk-numbers-parallel;125;5.287;12;algorithm=base,faithful=yes,bits=64

Passes: 131, Time: 5.443, Avg: 0.041549618321, Limit: 1000000, Count: 78498, Valid: true
willeyeuk-bitset-parallel;131;5.443;12;algorithm=base,faithful=yes,bits=1

Passes: 211, Time: 5.189, Avg: 0.024592417062, Limit: 1000000, Count: 78498, Valid: true
willeyeuk-booleans-parallel;211;5.189;12;algorithm=base,faithful=yes,bits=64

```
