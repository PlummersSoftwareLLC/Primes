# V solution by Penguindark,Ekopalypse ...

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Run

### Run locally

You can find an installation guide on the official website at https://vlang.io/.
Basically call v -cc gcc -prod run primes.v 1_000_000

### Docker

The official images have no support for ARM64 therefor; this Dockerfile also installs the latest version of V.

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks

```
Passes: 4978, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 4967, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 4969, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 4967, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 4914, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 4909, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 4962, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 4962, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 4964, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 4959, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```
