# Bun solution by Dief Bell
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
<!-- ![Parallelism](https://img.shields.io/badge/Parallel-yes-green) -->

Implementation in Bun, with a 32 bit integer array as buffer for the bit array.
This implementation essentially a refactored copy of the existing NodeJS implementation
by Rogier van Dam (as of commit 61ab3fa9576fee399b8d4eac9af14e2e2d2c54fe),
which was based on the logic from:
- previous implementation of NodeJS/solution_1 by Frank van Bakel
- Python/solution_2                            by ssovest
- PrimeCPP                                     by Dave Plummer

**PrimeBun.js** -The base algorithm follows the orginal one by Dave Plummer, the only exception being that the factors themselves are divided by 2 at the start.

<!-- **PrimeBun_cluster.js** - Multiprocessor version using cluster API, running batches of sieves on each processor. -->

**PrimeBun_memcopy** - New algorithm in which the product of a prime is only marked until a certain block range is reached. Thereafter, a block copy algorithm takes over and the recurring pattern 4 bytes at a time to different offsets, carefully trying to keep using cpu cache level 1. It follows the basic rules, but is not "base", because it does not follow the rule "*When clearing non-primes in the sieve (the second operation), the algorithm clears all non-primes individually, increasing the number with 2 * factor on each cycle.*"

## Run instructions
Install Bun (Unix or Windows Subsystem for Linux only): <https://bun.sh>

```bash
cd path/to/sieve
bun PrimeBun.js
bun PrimeBun_memcopy.js
```

## Output
Below is an example of the output

```bash
diefbell;9955;5.000525147;1;algorithm=base,faithful=yes,bits=1
diefbell_memcopy;13214;5.000256333;1;algorithm=other,faithful=yes,bits=1
```
