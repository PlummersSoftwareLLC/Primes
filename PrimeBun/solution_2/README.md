# Bun solution by Dief Bell
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
<!-- ![Parallelism](https://img.shields.io/badge/Parallel-yes-green) -->

Implementation in Bun using TypeScript, with a 32 bit integer array as buffer for the bit array. See the Bun project at https://bun.sh

This solution tests the output of both Bun's in-built TypeScript transpiler and TypeScript's tsc compiler.

This implementation is a TypeScriptified version of solution_1, which in turn is
essentially a refactored copy of the existing NodeJS implementation
by Rogier van Dam (as of commit 61ab3fa9576fee399b8d4eac9af14e2e2d2c54fe),
which was based on the logic from:
- previous implementation of NodeJS/solution_1 by Frank van Bakel
- Python/solution_2                            by ssovest
- PrimeCPP                                     by Dave Plummer

**PrimeBun.ts** -The base algorithm follows the orginal one by Dave Plummer, the only exception being that the factors themselves are divided by 2 at the start.

<!-- **PrimeBun_cluster.ts** - Multiprocessor version using cluster API, running batches of sieves on each processor. -->

**PrimeBun_memcopy.ts** - New algorithm in which the product of a prime is only marked until a certain block range is reached. Thereafter, a block copy algorithm takes over and the recurring pattern 4 bytes at a time to different offsets, carefully trying to keep using cpu cache level 1. It follows the basic rules, but is not "base", because it does not follow the rule "*When clearing non-primes in the sieve (the second operation), the algorithm clears all non-primes individually, increasing the number with 2 * factor on each cycle.*"

## Run instructions
Install Bun (Unix or Windows Subsystem for Linux only): <https://bun.sh>

```bash
cd path/to/sieve
bun install
bun run tsc ./PrimeBun.ts ./PrimeBun_memcopy.ts
bun PrimeBun.ts
bun PrimeBun_memcopy.ts
BUN_DISABLE_TRANSPILER=1 bun PrimeBun.js
BUN_DISABLE_TRANSPILER=1 bun PrimeBun_memcopy.js
```

## Output
Below is an example of the output

```bash
diefbell_ts-bun;30825;15.000290034;1;algorithm=base,faithful=yes,bits=1
diefbell_ts-bun_memcopy;44650;15.000048989;1;algorithm=other,faithful=yes,bits=1
diefbell_tsc;30166;15.000311749;1;algorithm=base,faithful=yes,bits=1
diefbell_tsc_memcopy;44111;15.000320353;1;algorithm=other,faithful=yes,bits=1
```
