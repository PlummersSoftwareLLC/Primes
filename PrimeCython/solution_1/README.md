# Cython Prime Sieve by rpkak

![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is a copy of the [second implementation of the python prime sieve](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimePython/solution_2) with cython implemented.

There are three versions of the program:

 * `PrimeCY.pyx` is a copy of the Python version with minimal changes, such as
   adding types, showing what Cython can do for the performance of already fast
   Python code with minimal effort.
 * `PrimeCY_pointers.pyx` is a version that uses C-level pointers to access the bit
   array rather than Python-level functions
 * `PrimeCY_bitarray.pyx` stores flags as individual bits rather than as bytes.

## Run instructions

To build:

    pip3 install cython
    sh buildall.sh

To run the different versions:

    ./PrimeCY
    ./PrimeCY_pointers
    ./PrimeCY_bitarray

You may not have the Python development headers and library installed. It may be
easier to use docker:

```
docker build . -t primecy
docker run --rm primecy
```

## Command line arguments

 - `--limit=X`, `-l X`: set upper limit for calculating primes. Default is 1_000_000.
 - `--time=X`, `-t X`: set running time, in seconds. Default is 10.
 - `--show`, `-s`: output the found primes.

# Output

```
Passes: 4996, Time: 5.000531282001248, Avg: 0.0010009069819858383, Limit: 1000000, Count: 78498, Valid: True

rpkak; 4996;5.000531282001248;1;algorithm=base,faithful=yes,bits=8
```

# Cython and Python compared

| Limit | Cython (minimal changes) | Python (original) | Cython with pointers | Cython with bit field |
|-----------|------------|-----------|------------|-----------|
| 10        | 4_131_936  | 1_873_498 | 11_478_593 | 8_335_287 |
| 100       | 1_796_127  | 783_782   | 10_084_268 | 7_046_574 |
| 1000      | 833_749    | 373_348   | 6_616_337  | 5_129_540 |
| 10000     | 227_157    | 139_545   | 1_835_678  | 1_115_175 |
| 100000    | 58_797     | 39_563    | 180_550    | 113_076   |
| 1000000   | 5_079      | 2_364     | 13_800     | 10_027    |
| 10000000  | 284        | 209       | 856        | 857       |
| 100000000 | 10         | 11        | 13         | 59        |