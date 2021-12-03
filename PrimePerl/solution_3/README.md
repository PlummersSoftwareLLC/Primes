# Perl solution using PDL by Luis Mochan

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

My implementation uses the Perl Data Language (PDL) to mark all multiples of
a prime with a single instruction. PDL is a package that extends Perl,
allowing relatively fast number-crunching using objects that contain
C-like arrays without loosing Perl's expressiveness. Thus it loads two
external packages and it is not pure Perl.


## Run

### Running locally

Perl usually comes preinstalled on a Linux system; if it's missing
from your machine, check your package manager. For Windows, there's
ActivePerl or Strawberry Perl. If PDL is also installed, then the
program may by run as

```
perl primes.pl
```

### Docker

No local Perl or PDL? Just run it in Docker.

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks

In my laptop it's about an order of magnitude faster than Perl's
solution 1. I did use external modules, but they are general purpose
language extensions, not targeted to this problem.

On my Dell Latitude E7450, I get the following numbers:

```
Passes: 459, Time: 5.00613212585449, Per pass: 0.0109066059386808 Limit: 1000000 Count: 78498 Valid: 1
Passes: 442, Time: 5.00916695594788, Per pass: 0.0113329569139092 Limit: 1000000 Count: 78498 Valid: 1
Passes: 441, Time: 5.00524806976318, Per pass: 0.0113497688656762 Limit: 1000000 Count: 78498 Valid: 1
Passes: 456, Time: 5.00585103034973, Per pass: 0.0109777434876091 Limit: 1000000 Count: 78498 Valid: 1
Passes: 448, Time: 5.00958800315857, Per pass: 0.0111821160784789 Limit: 1000000 Count: 78498 Valid: 1
```

## Author

Luis Mochán
