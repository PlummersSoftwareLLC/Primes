# Swift solutions by j-f1, yellowcub

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This is a collection of implementations in Swift, of which:

1. one is a "naive" implementation that maintains the array of primes in an underlying array of 8-bit booleans
2. one is a "naive" implementation that maintains the array of primes in an underlying array of bit-mapped 8-bit unsigned integers
3. one is a "striped" implementation that maintains the array of primes in an underlying array of bit-mapped 8-bit unsigned integers

Credits:

1. j-f1 - Original implementation and primary code arrangement
2. yellowcub - performance improvements, 1-bit implementation for leaderboard

## Run instructions

Shell script `./run.sh` runs both solutions sequentially.

## Output

  CC(target) Release/obj.target/uname/uname.o
  SOLINK_MODULE(target) Release/uname.node
added 231 packages in 5.407s
info: Unconfined mode: false
info: Detected architecture: amd64
info: [PrimeSwift][solution_1] Building...
info: [PrimeSwift][solution_1] Running...
                                                            Single-threaded
┌───────┬────────────────┬──────────┬─────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label               │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼─────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ swift          │ 1        │ yellowcub_bit32     │  7338  │ 5.00008  │    1    │   base    │   yes    │ 1    │  1467.57505   │
│   2   │ swift          │ 1        │ yellowcub_bit64     │  6918  │ 5.00010  │    1    │   base    │   yes    │ 1    │  1383.57150   │
│   3   │ swift          │ 1        │ j-f1_yellowcub_bool │  6881  │ 5.00071  │    1    │   base    │   yes    │ 8    │  1376.00408   │
└───────┴────────────────┴──────────┴─────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
