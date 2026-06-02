# CFML (Lucee) solution 2 by willeyeuk

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)


Multi-threaded solution writen for Lucee 7 (CFML).

## Build and run instructions

```shell
docker build -t primes-cfml2 .
docker run --rm primes-cfml2
```

## Output

```shell
Passes: 172, Time: 5.224000, Avg: 0.030372, Limit: 1000000, Count: 78498, Valid: true 
willeyeuk-threaded;172;5.224000;16;algorithm=base,faithful=yes,bits=1
```
