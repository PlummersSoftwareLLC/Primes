# V implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Run

### Run locally

You can find an installation guide on the official website at https://vlang.io/.

### Docker

The official images have no support for ARM64 therefor; this Dockerfile also installs the latest version of V.

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks

```
Passes: 782, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 811, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 858, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 858, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 860, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 853, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 854, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 853, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 857, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 850, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```

## Notes

This language is still in early development, with a lot of features missing. I bumped into quite a few issues while putting this together; honestly, this wasn't a pretty experience, and I don't think this is useable in any production environment. The language wants to look like Golang, but it's far from it.

Nevertheless, this is my take on it; if anybody has more experience with V and can improve the performance and/or coding style, I would be curious to see it. Performance-wise it's not the best nor the worst either.

## Author

Tudor Marghidanu
https://marghidanu.com/