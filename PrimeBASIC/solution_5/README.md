# TI-84 Basic Solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This version is for the TI-84 graphing calculator. It uses a 
[python-based simulator](https://github.com/rzuckerm/pitybas) that is a fork
of [this repo](https://github.com/lunixbochs/pitybas). This program
on a real TI-84 would most likely not handle a sieve size much larger than
a few thousand due to memory constraints.

Note that TI-Basic has no classes or even subroutines, so the code in just
one big function. Therefore, the reason why this solution is marked as
unfaithful is that TI-Basic lacks any way to encapsulate the sieve and its
related data in a data structure.

In this implementation, each array element indicates whether the value is
prime (0) or composite (1). The array only holds odd values from 3 to the
sieve size. Also, the TI-84 only has a clock that measures hours, minutes,
and seconds, where all values are an integer. Therefore, the duration is
truncated to a one-second increment.

## Run instructions

Build the docker image with this:

```bash
./build.sh
```

You should only need to do this once. Run the docker image:

```bash
./run.sh
```

`run.sh` has three optional command-line arguments:

- 1: Sieve size (default=1000000)
- 2: Time limit in sec (default=5)
- 3: Show results (default=0), where a non-zero value means do not show results

## Output

On a 12th Gen Intel(R) Core(TM) i7-1255U 1.70 GHz with 16 GB of memory on a Windows 11
laptop running Ubuntu 22.04 in WSL2:

```console
./run.sh 10
Passes: 11313, Time: 5, Avg: 0.00044196941571643, Limit: 10, Count=4, Valid=1
rzuckerm-ti-basic;11313;5;1;algorithm=base,faithful=no

./run.sh 100
Passes: 2766, Time: 5, Avg: 0.0018076644974693, Limit: 100, Count=25, Valid=1
rzuckerm-ti-basic;2766;5;1;algorithm=base,faithful=no

./run.sh 1000
Passes: 309, Time: 5, Avg: 0.016181229773463, Limit: 1000, Count=168, Valid=1
rzuckerm-ti-basic;309;5;1;algorithm=base,faithful=no

./run.sh 10000
Passes: 28, Time: 5, Avg: 0.17857142857143, Limit: 10000, Count=1229, Valid=1
rzuckerm-ti-basic;28;5;1;algorithm=base,faithful=no

./run.sh 100000
Passes: 3, Time: 6, Avg: 2, Limit: 100000, Count=9592, Valid=1
rzuckerm-ti-basic;3;6;1;algorithm=base,faithful=no

./run.sh 1000000
Passes: 1, Time: 24, Avg: 24, Limit: 1000000, Count=78498, Valid=1
rzuckerm-ti-basic;1;24;1;algorithm=base,faithful=no
```
