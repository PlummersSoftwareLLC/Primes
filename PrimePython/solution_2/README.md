# Python Prime Sieve Improvements by jackguo

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

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

# Output on Mac x86 2,3 GHz Intel Core i5

```
$ python3 PrimePY.py 
Passes: 4224, Time: 10.000919606, Avg: 0.00236764195217803, Limit: 1000000, Count: 78498, Valid: True

ssovest; 4224;10.000919606;1;algorithm=base,faithful=yes,bits=8
```
