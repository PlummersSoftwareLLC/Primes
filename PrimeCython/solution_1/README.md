# Cython Prime Sieve by rpkak

![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This is a copy of the [second implementation of the python prime sieve](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimePython/solution_2) with cython implemented.

## Run instructions

```
pip install cython
cython -3 --embed PrimeCY.pyx -o PrimeCY.c
gcc -I /usr/local/include/python3.9 -l python3.9 -lm PrimeCY.c -o PrimeCY
./PrimeCY
```

Belonging to your python installation the python include path may be different and the linker may not found the library by default. So to avoid these problems you can use docker:

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
Passes: 3592, Time: 10.001709000000119, Avg: 0.00278444014476618, Limit: 1000000, Count: 78498, Valid: True

ssovest; 3592;10.001709000000119;1;algorithm=base,faithful=yes,bits=8
```

# Cython and Python compared

| Limit | Passes in Cython | Passes in Python |
|-|-|-|
| 10 | 2093636 | 982066 |
| 100 | 939333 | 461066 |
| 1000 | 396187 | 206818 |
| 10000 | 112930 | 70036 |
| 100000 | 17217 | 8741 |
| 1000000 | 1836 | 1022 |
| 10000000 | 63 | 45 |
| 100000000 | 5 | 5 |