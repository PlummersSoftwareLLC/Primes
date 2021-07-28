# Prime Sieve Algorithms

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Java solutions by @chrvanorle

This is a collection of prime sieve algorithms implemented in Java. All solutions only store odd prime candidates, and use inverted values to avoid initializing the array to ones. The implementations are:

* I8: Using bytes to store primes, 1 bit per prime
* I32: Using 32 bit integers to store primes, 1 bit per prime
* I64: Using 64 bit integers to store primes, 1 bit per prime

## Building and running

To compile from the commandline, run `javac src/*.java`. Run by using one of the lines in `runSolution.sh`

## Options

Options you can set from the commandline:

* -parallel: Run multithreaded.
* -threads: How many threads to use when running multithreaded. Defaults to the number of processors on the system.

## Output

Results:

i7-8750H
```
chrvanorleI8;4026;5.006000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32;6017;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64;5867;5.000000;1;algorithm=base,faithful=yes,bits=1
```

i5-3570K
```
chrvanorleI8;4319;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32;6335;5.006000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64;6130;5.009000;1;algorithm=base,faithful=yes,bits=1
```

e5-2670 (V1)
```
chrvanorleI8;2806;5.001000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32;3735;5.001000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64;3604;5.000000;1;algorithm=base,faithful=yes,bits=1
```

r7 3700x (windows 10)
```
chrvanorleI8;4775;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32;6884;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64;6367;5.001000;1;algorithm=base,faithful=yes,bits=1
```

r7 3700x (centos 8)
```
chrvanorleI8;5959;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32;8972;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64;7788;5.000000;1;algorithm=base,faithful=yes,bits=1
```