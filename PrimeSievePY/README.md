# Python Prime Sieve

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
