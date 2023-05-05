# JavaScript solution by rogiervandam and Dief Bell
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Implementation in JavaScript, with a 32 bit integer array as buffer for the bit array.
NodeJS has been implemented for all files, and some run on Deno and Bun too.

This implementation is based on the logic from:
- previous implementation of NodeJS/solution_1 by Frank van Bakel
- Python/solution_2                            by ssovest
- PrimeCPP                                     by Dave Plummer

<br />


**PrimeJavaScript.js** -The base algorithm follows the orginal one by Dave Plummer, the only exception being that the factors themselves are divided by 2 at the start.
- NodeJS &check;
- Deno &check;
- Bun &check;

<br />


**PrimeJavaScript_cluster.js** - Multiprocessor version using cluster API, running batches of sieves on each processor.
- NodeJS &check;
- Deno &cross;
- Bun &cross;

<br />


**PrimeJavaScript_worker_main.mjs** - Multiprocessor version using the worker_threads API, running batches of sieves on each processor.
- NodeJS &check;
- Deno &cross;
- Bun &cross;

<br />


**PrimeJavaScript_memcopy** - New algorithm in which the product of a prime is only marked until a certain block range is reached. Thereafter, a block copy algorithm takes over and the recurring pattern which copies 4 bytes at a time to different offsets, carefully trying to keep using cpu cache level 1. It follows the basic rules, but is not "base", because it does not follow the rule "*When clearing non-primes in the sieve (the second operation), the algorithm clears all non-primes individually, increasing the number with 2 * factor on each cycle.*"
- NodeJS &check;
- Deno &cross;
- Bun &check;

<br />


## Run instructions
Install NodeJS: <https://nodejs.org/en/download/>
Install Bun: <https://bun.sh>
Install Deno: <https://deno.land/manual/getting_started/installation>

To run all:  `cd <path_to_sieve> && ./run.sh`.

To run individually:
```bash
cd path/to/sieve
node PrimeJavaScript.js
bun PrimeJavaScript.js
deno run PrimeJavaScript.js
# etc
```

<br />


## Output
Below is an example of the output

```bash
rogiervandam-node;7868;5.000452585998922;1;algorithm=base,faithful=yes,bits=1
rogiervandam-bun;10231;5.000154136;1;algorithm=base,faithful=yes,bits=1
rogiervandam-deno;6935;5;1;algorithm=base,faithful=yes,bits=1
```
