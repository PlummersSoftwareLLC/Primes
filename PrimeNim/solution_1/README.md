# Nim implementation

![Category](https://img.shields.io/badge/Category-faithful-green)

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

Running locally on my M1 MacbookAir, I get some astounding numbers:

```
Passes: 8912, Time: 5.000054121017456, Avg: 0.0005610473654642567, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8890, Time: 5.000402927398682, Avg: 0.0005624750199548573, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8833, Time: 5.000484943389893, Avg: 0.0005661139978931158, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8830, Time: 5.000242948532104, Avg: 0.0005662789296185848, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8696, Time: 5.000200986862183, Avg: 0.0005750001134846116, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8704, Time: 5.000247955322266, Avg: 0.0005744770169258118, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8826, Time: 5.000135898590088, Avg: 0.0005665234419431326, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8897, Time: 5.000159978866577, Avg: 0.000562005167906775, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8666, Time: 5.000056982040405, Avg: 0.0005769740343919231, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 8756, Time: 5.000047922134399, Avg: 0.0005710424762602101, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```

## Notes

I don't know about this language, some parts I like, some I dislike—overall it shows good performance on my machine.

## Author

Tudor Marghidanu
https://marghidanu.com/