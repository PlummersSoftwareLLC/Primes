# M4 solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Introduction

M4 is a macro processing language developed by Brian Kernighan and Dennis Ritchie,
the creators of the C programming language. Macros are used instead of
the traditional functions that are in most languages. Macros also may be used as
variables. M4 has no loops, but loops may implemented with recursive macros. M4
has some annoying limitations:

- Output can only be written to stdout.
- Input can only be done by defining macros on the command line.
- There are no timers. However, system commands like `date` or `cat /proc/uptime`
  can simulate this capability. The latter is used in this solution.
- There is no floating-point support. All mathematical operations must be done
  with 32-bit signed integers.

This solution is marked as "unfaithful" since M4 does not have any type of
data structures.

There are two solutions:

- `primes.m4` - Boolean implementation. Sieve is initialized by defining the
  `s2`, `s3`, `s5`, `s7`, ... macros to the value of `1`. In other words, 2
  and each odd factor from 3 to the specified limit (inclusive) are defined.
  All odd composite factors are eliminated by deleting the macro. At the end,
  only prime factors remain.
- `primes-bit.m4` - 1-bit implementation. Sieve is initialized by defining the
  `s0`, `s1`, `s2`, ... macros to the value of all zeros up to the number of
  32-bit words needed to represent the specified limit:
   - `s0`:
     - 3: bit 0
     - 5: bit 1
     - ...
     - 65: bit 31
   - `s1`:
     - 67: bit 0
     - 69: bit 1
     - ...
     - 129: bit 31
   - ...
   - `s<N-1>`:
     - 64*(`N` - 1) + 3: bit 0
     - 64*(`N` - 1) + 5: bit 1
     - ...
     - 64*(`N` - 1) + 65: bit 31

  where `N` is the number of words needed represent odd factors from 3 to the
  specified limit, inclusive.

  All odd composite factors are eliminated by setting a bit in a word
  corresponding to the factor.

For further details on the M4 language, see this
[Wikipedia article](https://en.wikipedia.org/wiki/M4_(computer_language)).
Also, see the [GNU M4 Manual](https://www.gnu.org/software/m4/manual/m4.html).

## Run instructions

Build the docker image with this:

```bash
./build.sh
```

You should only need to do this once. Run the docker image:

```bash
./run.sh
```

## Command-line arguments

You can add the following command-line arguments to `run.sh`:

- `-DLIMIT=<limit>` - Upper limit for calculating prime numbers. Default: 1000000
- `-DTIME=<time>` - Time limit in seconds. Default: 5
- `-DSHOW_RESULTS=1` - Print found prime numbers

## Output

On a 12th Gen Intel(R) Core(TM) i7-1255U 1.70 GHz with 16 GB of memory on a Windows 11
laptop running Ubuntu 22.04 in WSL2:

```console
$ ./run.sh -DLIMIT=10
Passes: 3265, Time: 5.00, Avg: 653.000, Count: 4, Valid: true
rzuckerm-m4;3265;5.00;algorithm=base;faithful=no

Passes: 3231, Time: 5.00, Avg: 646.200, Count: 4, Valid: true
rzuckerm-m4-bit;3231;5.00;algorithm=base;faithful=no,bits=1

$ ./run.sh -DLIMIT=100
Passes: 2854, Time: 5.00, Avg: 570.800, Count: 25, Valid: true
rzuckerm-m4;2854;5.00;algorithm=base;faithful=no

Passes: 2647, Time: 5.00, Avg: 529.400, Count: 25, Valid: true
rzuckerm-m4-bit;2647;5.00;algorithm=base;faithful=no,bits=1

$ ./run.sh -DLIMIT=1000
Passes: 1360, Time: 5.00, Avg: 272.000, Count: 168, Valid: true
rzuckerm-m4;1360;5.00;algorithm=base;faithful=no

Passes: 934, Time: 5.00, Avg: 186.800, Count: 168, Valid: true
rzuckerm-m4-bit;934;5.00;algorithm=base;faithful=no,bits=1

$ ./run.sh -DLIMIT=10000
Passes: 224, Time: 5.00, Avg: 44.800, Count: 1229, Valid: true
rzuckerm-m4;224;5.00;algorithm=base;faithful=no

Passes: 105, Time: 5.01, Avg: 20.958, Count: 1229, Valid: true
rzuckerm-m4-bit;105;5.01;algorithm=base;faithful=no,bits=1

$ ./run.sh -DLIMIT=100000
Passes: 21, Time: 5.04, Avg: 4.166, Count: 9592, Valid: true
rzuckerm-m4;21;5.04;algorithm=base;faithful=no

Passes: 9, Time: 5.01, Avg: 1.796, Count: 9592, Valid: true
rzuckerm-m4-bit;9;5.01;algorithm=base;faithful=no,bits=1

$ ./run.sh -DLIMIT=1000000
Passes: 2, Time: 6.11, Avg: 0.327, Count: 78498, Valid: true
rzuckerm-m4;2;6.11;algorithm=base;faithful=no

Passes: 1, Time: 6.51, Avg: 0.153, Count: 78498, Valid: true
rzuckerm-m4-bit;1;6.51;algorithm=base;faithful=no,bits=1

$ ./run.sh -DLIMIT=10000000
Passes: 1, Time: 56.94, Avg: 0.017, Count: 664579, Valid: true
rzuckerm-m4;1;56.94;algorithm=base;faithful=no

Passes: 1, Time: 84.71, Avg: 0.011, Count: 664579, Valid: true
rzuckerm-m4-bit;1;84.71;algorithm=base;faithful=no,bits=1
```
