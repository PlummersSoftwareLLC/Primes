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

`__$ make DIRECTORY=PrimeSwift/solution_1
  CC(target) Release/obj.target/uname/uname.o
  SOLINK_MODULE(target) Release/uname.node
added 229 packages in 4.16s
info: Detected architecture: amd64
info: [PrimeSwift][solution_1] Building...
info: [PrimeSwift][solution_1] Running...
                                                           Single-threaded                                                           
┌───────┬────────────────┬──────────┬───────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label             │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼───────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ PrimeSwift     │ 1        │ yellowcub_boolean │  6884  │ 5.00020  │    1    │   base    │   yes    │ 8    │  1376.74381   │
│   2   │ PrimeSwift     │ 1        │ yellowcub_bit64   │  3459  │ 5.00117  │    1    │   base    │   yes    │ 1    │   691.63802   │
└───────┴────────────────┴──────────┴───────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
`