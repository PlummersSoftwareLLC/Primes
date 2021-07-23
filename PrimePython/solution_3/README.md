# Numpy Prime Sieve by emillynge

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This solution is more or less copied from solution2  (including tests + Readme) with the one change that this version 
uses a numpy array to store the sieve.
Marking numbers as non-prime in the sieve seems to be the most demanding operation in this algorithm,
and numpy is very well optimized for that sort of task

The solution is **unfaithful** due to the inclusion of a dependency. *But* due to how incredibly 
widespread numpy is, I felt it was an important inclusion showing off how much performance can be gained by
using one of the many brilliant C-extension based packages in the python eco-system.
 

## Running with Python

Install Python: https://www.python.org/downloads/


```
cd path/to/sieve
python PrimePY.py
```

## Running with Pypy

Download and extract Pypy3: https://www.pypy.org/download.html


```
cd path/to/pypy
pypy3 path/to/sieve/PrimePY.py
```

## Command line arguments

 - `--limit=X`, `-l X`: set upper limit for calculating primes. Default is 1_000_000.
 - `--time=X`, `-t X`: set running time, in seconds. Default is 10.
 - `--show`, `-s`: output the found primes.

## Running tests

```
cd path/to/sieve
python -m unittest
```

# Results on my machine

 - AMD Ryzen 3600, Arch Linux 64 bit
 - Python: 3.9.5 64 bit
 - PyPy: 7.3.5
 - g++: 11.1.0


Report with select other solutions:
```
                                                            Single-threaded                                                             
┌───────┬────────────────┬──────────┬──────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼──────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ c              │ 1        │ mckoss-c830          │ 12469  │ 5.00000  │    1    │   wheel   │   yes    │ 1    │  2493.80000   │
│   2   │ python         │ 3        │ emillynge_numpy      │  7830  │ 5.00030  │    1    │   base    │    no    │ 8    │  1565.90684   │
│   3   │ python         │ 3        │ emillynge_numpy_pypy │  4140  │ 5.00031  │    1    │   base    │    no    │ 8    │   827.94845   │
│   4   │ python         │ 2        │ ssovest              │  2179  │ 5.00037  │    1    │   base    │   yes    │ 8    │   435.76803   │
│   5   │ python         │ 1        │ davepl               │   40   │ 10.05578 │    1    │   base    │   yes    │      │    3.97781    │
└───────┴────────────────┴──────────┴──────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```

pypy result included, but no Dockerfile has been provided, so that result will not become part of the regular report.

C report hasbeen included due to numpy being mostly a C "program".

# Example
```
$ python3 PrimePython/solution_3/PrimePY.py 
Passes: 10392, Time: 5.0000652491580695, Avg: 0.00048114561673961407, Limit: 1000000, Count: 78498, Valid: True

emillynge_numpy; 10392;5.0000652491580695;1;algorithm=base,faithful=no,bits=8
```