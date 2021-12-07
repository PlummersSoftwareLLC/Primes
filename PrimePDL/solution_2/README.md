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
Luis_Mochán_(wlmb)_Perl/PDL-PP;1091;5.003844;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL-PP;1063;5.004238;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL-PP;1067;5.003807;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL-PP;1065;5.000665;1;algorithm=base,faithful=yes,bits=8
Luis_Mochán_(wlmb)_Perl/PDL-PP;1043;5.001787;1;algorithm=base,faithful=yes,bits=8
```

### Benchmarks

In my laptop it's more than thirty times faster than Perl's
solution 1. I did use external modules, but they are general purpose
language extensions, not targeted to this problem. I used the PDL's pp_def
mechanism to write threadable C-like functions.

On my Dell Latitude E7450, I get the following numbers:

```
Passes: 1091, Time: 5.0038, Per pass: 4.586e-03 Limit: 1000000, Count: 78498, Valid: 1
Passes: 1063, Time: 5.0042, Per pass: 4.708e-03 Limit: 1000000, Count: 78498, Valid: 1
Passes: 1067, Time: 5.0038, Per pass: 4.690e-03 Limit: 1000000, Count: 78498, Valid: 1
Passes: 1065, Time: 5.0007, Per pass: 4.695e-03 Limit: 1000000, Count: 78498, Valid: 1
Passes: 1043, Time: 5.0018, Per pass: 4.796e-03 Limit: 1000000, Count: 78498, Valid: 1
```

## Author

Luis Mochán
