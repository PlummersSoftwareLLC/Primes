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
* I32CUnroll: Using 32 bit integers to store primes, 1 bit per prime, unrolling the loop by 4.
* Strided32Blocks: Using 32 bit integers to store primes, 1 bit per prime, unrolled, and using the repeating pattern of bitmasks (for `factor`, the mask will be repeat every `factor` bits in the dataSet) to go over the dataSet 32 times with 32 different bit patterns. This avoids calculating the mask per bit cleared. To alleviate the cache trashing that comes with this, the clearing is done in blocks of data that fit in most L1 caches. The mask always has a single bit set. We get better performance then I64PatternCalc, while now also being faithful base.

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
chrvanorleI32;6088;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32C;7252;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32CUnroll;8295;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64;5958;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64C;7077;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64PatternCalc;9227;5.000000;1;algorithm=other,faithful=yes,bits=1
chrvanorleI8;4926;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleStrided32Blocks16k;11505;5.000000;1;algorithm=base,faithful=yes,bits=1
```

r7-3700x (debian)
```
chrvanorleI32;4361;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32C;4468;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI32CUnroll;10224;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64;4398;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64C;4379;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleI64PatternCalc;6571;5.000000;1;algorithm=other,faithful=yes,bits=1
chrvanorleI8;4420;5.000000;1;algorithm=base,faithful=yes,bits=1
chrvanorleStrided32Blocks16k;16466;5.000000;1;algorithm=base,faithful=yes,bits=1
```
