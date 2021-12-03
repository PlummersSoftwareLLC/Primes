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
Luis_Mochán_(wlmb)_Perl/PDL;650;5.006607;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;657;5.000837;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;652;5.003785;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;650;5.001952;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;653;5.001199;1;algorithm=base,faithful=yes,bits=8
```

### Benchmarks

In my laptop it's about twenty times faster than Perl's
solution 1. I did use external modules, but they are general purpose
language extensions, not targeted to this problem.

On my Dell Latitude E7450, I get the following numbers:

```
Passes: 659, Time: 5.00056004524231, Per pass: 0.00758810325529941 Limit: 1000000 Count: 78498 Valid: 1
Passes: 632, Time: 5.00728106498718, Per pass: 0.00792291307751137 Limit: 1000000 Count: 78498 Valid: 1
Passes: 662, Time: 5.00472187995911, Per pass: 0.00756000283981738 Limit: 1000000 Count: 78498 Valid: 1
Passes: 652, Time: 5.00252103805542, Per pass: 0.0076725782792261 Limit: 1000000 Count: 78498 Valid: 1
Passes: 645, Time: 5.00771999359131, Per pass: 0.00776390696680823 Limit: 1000000 Count: 78498 Valid: 1
```

## Author

Luis Mochán
