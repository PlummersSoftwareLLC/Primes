# Perl implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Run

### Running locally

Perl usually comes preinstalled on a Linux system; if it's missing from your machine, check your package manager. For Windows, there's ActivePerl or Strawberry Perl.

```
perl primes.pl
```

### Docker

No local Perl? Just run it in Docker.

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks

It's slow, even though I tried a few optimizations. I also tried to avoid installing modules from CPAN since that wouldn't be fair as they might be implemented in C/C++. 

On my M1 MacBook Air, I get the following numbers:

```
Passes: 55, Time: 5.027288, Avg: 0.091405, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 56, Time: 5.070278, Avg: 0.090541, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 56, Time: 5.027602, Avg: 0.089779, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 56, Time: 5.064072, Avg: 0.090430, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 56, Time: 5.041186, Avg: 0.090021, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
```

## Author

Tudor Marghidanu
https://marghidanu.com/