# M4 solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Introduction

M4 is a macro processing language developed by Brian Kernighan and Dennis Ritchie,
the creators of the C programming language. Macros are used to the traditional
functions that are in most languages. Macros also may be used as variables.
M4 has no loops, but loops may implemented with recursive macros. M4 has some
annoying limitations:

- Output can only be written to stdout.
- Input can only be done by defining macros on the command line.
- There are no timers. However, commands like `date` or `cat /proc/uptime` can
  simulate this capability. The latter is used in this solution.
- There is no floating-point support. All mathematical operations must be done
  with 32-bit signed integers.

This solution is marked as "unfaithful" since M4 does not have any type of
data structures.

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
Passes: 1709, Time: 5.00, Avg: 341.800, Count: 4, Valid: true
rzuckerm-m4;1709;5.00;algorithm=base;faithful=no

$ ./run.sh -DLIMIT=100
Passes: 1584, Time: 5.00, Avg: 316.800, Count: 25, Valid: true
rzuckerm-m4;1584;5.00;algorithm=base;faithful=no

$ ./run.sh -DLIMIT=1000
Passes: 843, Time: 5.00, Avg: 168.600, Count: 168, Valid: true
rzuckerm-m4;843;5.00;algorithm=base;faithful=no

$ ./run.sh -DLIMIT=10000
Passes: 148, Time: 5.01, Avg: 29.540, Count: 1229, Valid: true
rzuckerm-m4;148;5.01;algorithm=base;faithful=no

$ ./run.sh -DLIMIT=100000
Passes: 13, Time: 5.03, Avg: 2.584, Count: 9592, Valid: true
rzuckerm-m4;13;5.03;algorithm=base;faithful=no

$ ./run.sh -DLIMIT=1000000
Passes: 2, Time: 9.34, Avg: 0.214, Count: 78498, Valid: true
rzuckerm-m4;2;9.34;algorithm=base;faithful=no

$ ./run.sh -DLIMIT=10000000
Passes: 1, Time: 95.14, Avg: 0.010, Count: 664579, Valid: true
rzuckerm-m4;1;95.14;algorithm=base;faithful=no
```
