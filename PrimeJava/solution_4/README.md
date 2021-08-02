# Prime Sieve Algorithms

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellow)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Java solutions by @chrvanorle

This is a collection of prime sieve algorithms implemented in Java. All solutions only store odd prime candidates, and use inverted values to avoid initializing the array to ones. The implementations are:

* I8: Using bytes to store primes, 1 bit per prime
* I32: Using 32 bit integers to store primes, 1 bit per prime
* I32C: Using 32 bit integers to store primes, 1 bit per prime, precache masks
* I64: Using 64 bit integers to store primes, 1 bit per prime
* I64C: Using 64 bit integers to store primes, 1 bit per prime, precache masks
* I64PatternCalc: Using 64 bit integers to store primes, 1 bit per prime, using masks to mark multiple values per array write, making it an "other" algorithm
* W appended means a warmup was done

## Building and running

To compile from the commandline, run `javac src/*.java`. Run by using one of the lines in `runSolution.sh`

## Options

Options you can set from the commandline:

* -parallel: Run multithreaded.
* -threads: How many threads to use when running multithreaded. Defaults to the number of processors on the system.
* -warmup: Runs the sieve a few seconds to warmup the JVM.

## Output

Results:

i7-8750H
```
chrvanorleI32W;6160;5.007000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32CW;7380;5.006000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64W;6041;5.005000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64CW;7171;5.013000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64PatternCalcW;9286;5.012000;1;algorithm=other,faithful=yes,bits=1
chrvanorleI8W;5367;5.012000;1;algorithm=base,faithful=yes,bits=1
```

e5-2670 (v1)
```
chrvanorleI32W;3735;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32CW;4358;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64W;3665;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64CW;4157;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64PatternCalcW;4999;5.005000;1;algorithm=other,faithful=yes,bits=1
chrvanorleI8W;3444;5.000000;1;algorithm=base,faithful=yes,bits=1
```

i5-3570k
```
chrvanorleI32W;5005;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32CW;5013;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64W;4855;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64CW;4771;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64PatternCalcW;7408;5.000000;1;algorithm=other,faithful=yes,bits=1
chrvanorleI8W;4123;5.000000;1;algorithm=base,faithful=yes,bits=1
```

r7-3700x (debian)
```
chrvanorleI32W;8428;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32CW;9269;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64W;8040;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64CW;7869;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64PatternCalcW;11819;5.000000;1;algorithm=other,faithful=yes,bits=1
chrvanorleI8W;3985;5.000000;1;algorithm=base,faithful=yes,bits=1
```
