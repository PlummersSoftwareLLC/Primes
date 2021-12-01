# NodeJS solution by rogiervandam
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Implementation in nodeJS, with a 32 bit integer array as buffer for the bit array.
This implementation is based on the logic from:
- previous implementation of NodeJS/solution_1 by Frank van Bakel
- Python/solution_2                            by ssovest
- PrimeCPP                                     by Dave Plummer

**PrimeNode.js** -The base algorithm follows the orginal one by Dave Plummer, the only exception being that the factors themselves are divided by 2 at the start.

**PrimeNode_cluster.js** - Multiprocessor version using cluster API, running batches of sieves on each processor.

**PrimeNode_memcopy** - New algorithm in which the product of a prime is only marked until a certain block range is reached. Thereafter, a block copy algorithm takes over and the recurring pattern 4 bytes at a time to different offsets, carefully trying to keep using cpu cache level 1. It follows the basic rules, but is not "base", because it does not follow the rule "*When clearing non-primes in the sieve (the second operation), the algorithm clears all non-primes individually, increasing the number with 2 * factor on each cycle.*"

## Run instructions
Install nodeJS: <https://nodejs.org/en/download/>

```bash
cd path/to/sieve
node PrimeNode.js
node PrimeNode_cluster.js
node PrimeNode_memcopy.js
```

## Output
Below is an example of the output

```bash
rogiervandam;5578;5.000737900003791;1;algorithm=base,faithful=yes,bits=1
rogiervandam;33316;5.0953555999994276;12;algorithm=base,faithful=yes,bits=1
rogiervandam_memcopy;9593;5.000124500006438;1;algorithm=other,faithful=yes,bits=1
```
