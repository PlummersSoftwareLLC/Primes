# Prime Sieve Algorithms

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)


## Java solutions by @Ciaccia

Just a straight-forward implementation of the sieve algorithm setting multiple bits at once using a bitmask.

The numbers are stored as bits inside 64-bit long primitives where `0` means prime and `1` non-prime. The backing array is accessed in the most straight-forward
possible way without intermediate getters and setters. The main goal of this implementation is to avoid multiple accesses to the same memory location in order
to achieve the highest possible performance.

## Building and running

To compile from the commandline, run `javac src/*.java`. Run by using one of the lines in `runSolution.sh`

## Options

No options, the warmup and validation are always executed.

## Output

Results:

Intel i7-8559U CPU @ 2.70GHz

```
BitmaskSieve;6391;5.000000;1;algorithm=base,faithful=yes,bits=1
```
