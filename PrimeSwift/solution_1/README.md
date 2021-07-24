# Swift solution by jf-1 modified by yellowcub

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This is a modified version of solution_1 by j-f1 with the following notable changes:
    * includes argument parser to specify time, sieve limit, and list results at run time.
    * in class PrimeSieveSwift corrected bits type to be BoolBitArray instead of BitArray (protocol), this fix had a significant increase on speed
    * on second thought, removed the extra structure layer of BoolBitArray - BoolBitArray was cleaner code, but we're all about speed here!
    * bits (formerly rawBits in BoolBitArray) to use UnsafeMutablePointer for efficient allocation/indexing
    * made Clock structure so final print results would not print a different time than what the while loop exited
    * modified runSieve - It seems for..in..stride is slower than a while loop, ... removing if branch increased performance as well
    * total speed is an order increase over original

## Run instructions

Stand alone run is demonstrated in ./run.sh

## Output

  CC(target) Release/obj.target/uname/uname.o
  SOLINK_MODULE(target) Release/uname.node
added 229 packages in 5.875s
info: Detected architecture: amd64
info: [PrimeSwift][solution_1] Building...
info: [PrimeSwift][solution_1] Running...
                                                       Single-threaded
┌───────┬────────────────┬──────────┬───────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label     │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼───────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ PrimeSwift     │ 1        │ yellowcub │  5159  │ 5.00019  │    1    │   base    │   yes    │ 8    │  1031.76057   │
└───────┴────────────────┴──────────┴───────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
