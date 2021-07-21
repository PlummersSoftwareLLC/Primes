# Cython Prime Sieve by rpkak

![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This prime sieve in Cython is based on [second implementation of the python prime sieve](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimePython/solution_2).

There are two versions of the program:

 * `PrimeCY_bytearray.pyx` uses one byte per flag, just like the Python version
 * `PrimeCY_bitarray.pyx` stores flags as individual bits rather than as bytes.

## Run instructions

To build:

    pip3 install cython
    sh buildall.sh

To run the different versions:

    ./PrimeCY_bytearray
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
Passes: 10209, Time: 5.000240805999965, Avg: 0.0004897875214026805, Limit: 1000000, Count: 78498, Valid: True

rpkak+bit-array; 10209;5.000240805999965;1;algorithm=base,faithful=yes,bits=1
```

# Cython and Python compared

| Limit | Python (original) | Cython (byte array) | Cython with (bit array) |
|-----------|-----------|------------|-----------|
| 10        | 1_873_498 | 11_478_593 | 8_335_287 |
| 100       | 783_782   | 10_084_268 | 7_046_574 |
| 1000      | 373_348   | 6_616_337  | 5_129_540 |
| 10000     | 139_545   | 1_835_678  | 1_115_175 |
| 100000    | 39_563    | 180_550    | 113_076   |
| 1000000   | 2_364     | 13_800     | 10_027    |
| 10000000  | 209       | 856        | 857       |
| 100000000 | 11        | 13         | 59        |
