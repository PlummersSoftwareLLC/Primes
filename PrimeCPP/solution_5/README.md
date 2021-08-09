# Original C++ solution by schmide

![Algorithm](https://img.shields.io/badge/Algorithm-other-green)
![Faithfulness]((https://img.shields.io/badge/Faithful-no-yellowgreen))
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

(this is a very rough outline)

SeiveBitOrs is a 2 pass implementation of the base algorithm.

The memory is allocated into 3 fields. 

bits - 1 bit per prime even numbers removed. (size half of limit)
replicators (see below)
indexors (see below)
indexes (not used yet future: subDivisions)

replicator replicates and ors words (QWords 8 bytes) (though lesser and greater words are possible they have not been tested),
Masking and shifting previously found primes into each word. This is valid for all bits less than half the size of the word.

indexor bits are mapped directly into bytes from a base bit.

seiveor actual implementation of the seive 

PrimeBuckets is for debugging

## Run instructions

(Linux): clang++ -Ofast -std=c++17 SeiveBitOrs.cpp -o Primes_clang++ && ./Primes_clang++

## Output

(note: output below is 3900x Ubuntu 20.04)

Passes: 18428, Time: 5.000000, Avg: 0.000271, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1

schmide;18428;5.000000;1;algorithm=base,faithful=yes,bits=1