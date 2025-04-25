# CFML (Lucee) solution by willeyeuk

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-64-yellowgreen)

Single-threaded solution writen for Lucee (CFML). It includes implementations using CFML Numbers and a Java BitSet.

## Build and run instructions

```shell
docker build -t primes-cfml1 .
docker run --rm primes-cfml1
```

## Output

```shell
Passes: 13, Time: 5.019, Avg: 0.386076923077, Limit: 1000000, Count: 78498, Valid: true
willeyeuk-numbers;13;5.019;1;algorithm=base,faithful=yes,bits=64

Passes: 14, Time: 5.255, Avg: 0.375357142857, Limit: 1000000, Count: 78498, Valid: true
willeyeuk-bitset;14;5.255;1;algorithm=base,faithful=yes,bits=1
```
