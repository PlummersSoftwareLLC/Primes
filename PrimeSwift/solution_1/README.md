# Swift solutions by j-f1, yellowcub

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This is a collection of implementations in Swift, of which:

1. one is a "naive" implementation that maintains the array of primes in an underlying array of 8-bit booleans
2. one is a "naive" implementation that maintains the array of primes in an underlying array of bit-mapped 64-bit unsigned integers

Credits:

1. j-f1 - Original implementation and primary code arrangement
2. yellowcub - performance improvements, 1-bit implementation for leaderboard

## Run instructions

Shell script `./run.sh` runs both solutions sequentially.

## Output

__$ ./run.sh
[0/0] Build complete!

Passes: 3864, Time: 5.000285983085632, Avg: 0.001294069871398973, Limit: 1000000, Count: 78498, Valid: true

yellowcub_bit64;3864;5.000285983085632;1;algorithm=base,faithful=yes,bits=1

[2/2] Build complete!

Passes: 6984, Time: 5.000074982643127, Avg: 0.0007159328440210663, Limit: 1000000, Count: 78498, Valid: true

j-f1_ycub_bool;6984;5.000074982643127;1;algorithm=base,faithful=yes,bits=8
