# Original C++ solution by davepl
# modified by Pol Marcet

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Run instructions

(Linux): clang++ -march=native -mtune=native -Ofast -pthread -std=c++17 PrimeCPP_PAR.cpp -o Primes_clang++ && ./Primes_clang++

## Output

Primes Benchmark (c) 2021 Dave's Garage - http://github.com/davepl/primes
-------------------------------------------------------------------------
Computing primes to 1000000 on 24 threads for 5 seconds.
Passes: 185267, Threads: 24, Time: 5.00074, Average: 2.69921e-05, Limit: 1000000, Counts: 78498/78498, Valid : Pass

davepl_par;185267;5.00074;24;algorithm=base,faithful=yes,bits=1
