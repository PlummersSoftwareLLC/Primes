# MATLAB solution by Brandon-Johns
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

MATLAB is a proprietary language and IDE designed mathematics computations. It is optimised for 'vectorised calculations', which is where the syntax is used to operate on entire matrices instead of individually addressing the matrix elements inside of loops. e.g. `5*A` multiplies every element of the matrix A by 5.

This implementation is based on the C++ solution 1. The sieve uses bitwise operations with the `uint8` type to consumes only 1 bit in memory per prime candidate. Doing so disallows using vectorisation and greatly increases computational time.

The solution is stored internally with inverted logic.

MATLAB's profiling tool `profile viewer` shows that performance currently is limited by the line which clears values of the bits array(actually it sets values, due to inverted logic), `this.bits(idx_byte) = bitset(this.bits(idx_byte), idx_bit, 1);`. This line consumes ~80% of execution time.

## Run instructions
Tested with MATLAB 2020b.

Run from cmd with
```cmd
matlab -singleCompThread -batch PrimesRun(<Sieve Size>,'<Output Option>')
```
`<Sieve Size>` (default 1,000,000) is the upper limit of primes to find
`<Output Option>`  (default 'basic') must be one of
* `basic`: Show only the minimum required output
* `stats`: Also show some extra stats, same as in the C++ solution 1
* `all`: Also show all of the calculated primes

E.G.
```cmd
matlab -singleCompThread -batch PrimesRun
matlab -singleCompThread -batch PrimesRun(1000000,'stats')
matlab -singleCompThread -batch PrimesRun(101,'all')
```

## Output
matlab -singleCompThread -batch PrimesRun
```cmd
Brandon-Johns_1bit;1;8.489951;1;algorithm=base,faithful=yes,bits=1
```

matlab -singleCompThread -batch PrimesRun(1000000,'stats')
```cmd
Passes: 1, Time: 8.456670, Avg: 8.456670, Limit: 1000000, Count: 78498, Valid: 1

Brandon-Johns_1bit;1;8.456670;1;algorithm=base,faithful=yes,bits=1
```

matlab -singleCompThread -batch PrimesRun(101,'all')
```cmd
2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101

Passes: 19688, Time: 5.000249, Avg: 0.000254, Limit: 101, Count: 26, Valid: 1

Brandon-Johns_1bit;19688;5.000249;1;algorithm=base,faithful=yes,bits=1
```

