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

Up to around twice as fast as solution_1 by doing:

* pull presetting of vars out of inner loops

* using a large string as bit-vector instead of a perl array

* perhaps a more idiomatic Perl style

On my computer solution_1 had 18 passes and solution_2 had 42.

## Author

Kjetil Skotheim, but algorithm copied from solution_1 by author Tudor Marghidanu