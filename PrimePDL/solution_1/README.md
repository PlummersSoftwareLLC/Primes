# Perl solution using PDL by Luis Mochan

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

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

## Output

```
Luis_Mochán_(wlmb)_Perl/PDL;726;5.001482;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;723;5.002705;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;721;5.003962;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;731;5.004047;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;714;5.005479;1;algorithm=base,faithful=yes,bits=8
```

### Benchmarks

In my laptop it's about twenty times faster than Perl's
solution 1. I did use external modules, but they are general purpose
language extensions, not targeted to this problem.

On my Dell Latitude E7450, I get the following numbers:

```
Passes: 726, Time: 5.0014820098877, Per pass: 0.00688909367753126 Limit: 1000000 Count: 78498 Valid: 1
Passes: 723, Time: 5.00270485877991, Per pass: 0.00691937048240651 Limit: 1000000 Count: 78498 Valid: 1
Passes: 721, Time: 5.00396203994751, Per pass: 0.00694030796109225 Limit: 1000000 Count: 78498 Valid: 1
Passes: 731, Time: 5.00404691696167, Per pass: 0.00684548141855222 Limit: 1000000 Count: 78498 Valid: 1
Passes: 714, Time: 5.00547909736633, Per pass: 0.00701047492628338 Limit: 1000000 Count: 78498 Valid: 1
```

## Author

Luis Mochán
