# Swift solution by yellowcub

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This is a modified version of solution_1 by j-f1 with the following notable changes:
    * includes argument parser to specify time, sieve limit, and list results at run time.
    * in class PrimeSieveSwift corrected bits type to be BoolBitArray instead of BitArray (protocol), this fix had a significant increase on speed
    * on second thought, removed the extra structure layer of BoolBitArray - cleaner code, but we're all about speed here!
    * bits (formerly rawBits in BoolBitArray) to use UnsafeMutablePointer for efficient allocation/indexing
    * made Clock structure so final print results would not print a different time than what the while loop exited
    * modified runSieve - It seems for..in..stride is slower than a while loop, ... removing if branch increased performance

## Run instructions

Stand alone run is demonstrated in ./run.sh

## Output

[*] Running primeswift-solution_2

Passes: 7216, Time: 5.000422954559326, Avg: 0.0006929632697560042, Limit: 1000000, Count: 78498, Valid: true

yellowcub;7216;5.000422954559326;1;algorithm=base,faithful=yes,bits=1

npm WARN prepare removing existing node_modules/ before installation
added 196 packages in 2.89s

> primes@0.1.0 start /Users/mortarsa/Documents/GitHub/Primes/tools
> ts-node ./src/index.ts "report" "-d" "/var/folders/q7/7s91xyhx5q1d062k6tpx10800000gn/T/tmp.4jsi686p" "-f" "table" "else" "echo" "Not specified!"

                                                       Single-threaded
┌───────┬────────────────┬──────────┬───────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label     │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼───────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ swift          │ 2        │ yellowcub │  7216  │ 5.00042  │    1    │   base    │   yes    │ 1    │  1443.07793   │
└───────┴────────────────┴──────────┴───────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
