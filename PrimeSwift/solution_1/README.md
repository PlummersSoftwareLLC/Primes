# Swift solutions by jf-1, yellowcub

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is a collection of implementations in Swift, of which:
1. one is a "naive" implementation that maintains the array of primes in an underlying array of 8-bit booleans
2. one is a "naive" implementation that maintains the array of primes in an underlying array of bit-mapped 64-bit unsigned integers

Credits:
1. jf-1 - Original implementation and primary code arrangement
2. yellowcub - performance improvements, 1-bit implementation for leaderboard

## Run instructions

Shell script `./run.sh` runs both solutions sequentially.

## Output

`__$ ./run.sh
[0/0] Build complete!

Passes: 3829, Time: 5.001450061798096, Avg: 0.001306202680020396, Limit: 1000000, Count: 78498, Valid: true

yellowcub_bit64;3829;5.001450061798096;1;algorithm=base,faithful=yes,bits=1

[0/0] Build complete!

Passes: 7005, Time: 5.000383973121643, Avg: 0.0007138306885255736, Limit: 1000000, Count: 78498, Valid: true

yellowcub;7005;5.000383973121643;1;algorithm=base,faithful=yes,bits=8
`