# Nim implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

## Run

### Run locally

Nim is available via package manager under the popular systems. The following commands should get you started:

```
nim c -d:release primes.nim
./primes
```

### Docker

As per usual minimal just a minimal set of commands:

```
docker build -t primes .
docker run --rm primes
```

I had to use the official Alpine images, which for now, have no support for ARM64. I will try to rectify this in the future.

## Benchmarks

Running locally on my M1 MacBook Air, I get some astounding numbers:

```
Passes: 8979, Time: 5.000272035598755, Avg: 0.0005568851804876662, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 9424, Time: 5.000049114227295, Avg: 0.0005305654832584142, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 9040, Time: 5.000542879104614, Avg: 0.0005531573981310413, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 9057, Time: 5.000289916992188, Avg: 0.000552091191011614, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8979, Time: 5.000046014785767, Avg: 0.0005568600083289639, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 9309, Time: 5.000034093856812, Avg: 0.0005371182827217544, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 9126, Time: 5.000533103942871, Avg: 0.0005479435792179346, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 9135, Time: 5.000319004058838, Avg: 0.0005473802960108197, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8687, Time: 5.000458955764771, Avg: 0.0005756255273126246, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8541, Time: 5.000423908233643, Avg: 0.0005854611764703948, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```

## Notes

I somehow feel that this is the type of language I would have a love-hate relationship.

## Author

Tudor Marghidanu
https://marghidanu.com/