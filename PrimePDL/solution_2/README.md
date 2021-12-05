# Perl solution using PDL by Luis Mochan

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

My implementation uses the Perl Data Language (PDL). PDL is a package that extends Perl,
allowing relatively fast number-crunching using objects that contain
C-like arrays without loosing Perl's expressiveness. For this solution
I use its pp_def mechanism that allows defining new threadable routines
using a C-like syntax and allowing them to be called from Perl. This
mechanism is used to define most of the PDL extensions of Perl, but it
can also be employed by the language users.


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
Luis_Mochán_(wlmb)_Perl/PDL;1020;5.001426;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;1023;5.003593;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;1048;5.001000;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;1037;5.002983;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL;1041;5.000536;1;algorithm=base,faithful=yes,bits=8
```

### Benchmarks

In my laptop it's about thirty times faster than Perl's
solution 1. I did use external modules, but they are general purpose
language extensions, not targeted to this problem. I used the PDL's
mechanism to write threadable C-like functions.

On my Dell Latitude E7450, I get the following numbers:

```
Passes: 1020, Time: 5.00142621994019, Per pass: 0.00490335903915704 Limit: 1000000 Count: 78498 Valid: 1
Passes: 1023, Time: 5.00359296798706, Per pass: 0.00489109772041746 Limit: 1000000 Count: 78498 Valid: 1
Passes: 1048, Time: 5.00099992752075, Per pass: 0.00477194649572591 Limit: 1000000 Count: 78498 Valid: 1
Passes: 1037, Time: 5.00298309326172, Per pass: 0.00482447742841053 Limit: 1000000 Count: 78498 Valid: 1
Passes: 1041, Time: 5.00053596496582, Per pass: 0.00480358882321404 Limit: 1000000 Count: 78498 Valid: 1
```

## Author

Luis Mochán
