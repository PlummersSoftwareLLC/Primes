# V solution 2 by GordonBGood

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Run

### Run locally

You can find an installation guide on the official website at https://vlang.io/.

### Docker

The official images have no support for ARM64 therefor; this Dockerfile also installs the latest version of V.

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks

As run on an Intel SkyLake i5-6500 at 3.6 GHz (single-threaded):

```
                                                                 Single-threaded                                                                 
┌───────┬────────────────┬──────────┬──────────────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                        │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼──────────────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ v              │ 2        │ GordonBGood_extreme-hybrid   │ 23969  │ 5.00020  │    1    │   base    │   yes    │ 1    │  4793.60634   │
│   2   │ v              │ 2        │ GordonBGood_extreme          │ 15922  │ 5.00019  │    1    │   base    │   yes    │ 1    │  3184.28154   │
│   3   │ v              │ 2        │ GordonBGood_stride8-block16K │ 13652  │ 5.00010  │    1    │   base    │   yes    │ 1    │  2730.34758   │
│   4   │ v              │ 2        │ GordonBGood_stride8          │ 10125  │ 5.00045  │    1    │   base    │   yes    │ 1    │  2024.81777   │
│   5   │ v              │ 2        │ GordonBGood_bittwiddle       │  6180  │ 5.00042  │    1    │   base    │   yes    │ 1    │  1235.89643   │
└───────┴────────────────┴──────────┴──────────────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```
Performance-wise it's not the best nor the worst either.

## Notes

The previous solution_1 just used an array of `bool` (probably 8-bit) as the sieve buffer; these techniques are all one bit.  The techniques uses are as follows:

1. The simplest is just the bit twidding one, with one small improvement in using a Look Up Table (LUT) for the bit masking patterns by bit index rather than using shifting.
2. The second technique uses striding in eight inner loops using a fixed mask for all bytes in the loop, for eight loops, each with a different mask value.  When there is "cache thrashing" as here, this often improves the performance due to the different cache line order, and also improves the performance because the inner loops are simpler.
3. Improving on the above technique, the next strided technique improves on the above by operation over a given block size at a time and thus slightly reducing the amount of "cache thrashing".
4. The fourth technique is that of "Extreme Loop Unrolling" where the eight "strided" loops are assembled into a single loop by recognizing the modulo marking patterns and using immediate values for the marking of each individual composite number bit; this can't use the block approach because it advances through the sieve buffer from start to finish.  In this implementation, as the V language does not have templates of macros, the loops are manually assembled, but this is made easier in that there are only four modulo eight patterns for odd primes starting at the base (each base prime modulo eight has a fixed marking start address, so only four patterns in total).
5. The final technique uses the above for base prime values above nineteen but for base primes below this threshold, it gives the dense marking treatment where it reads in a 64-bit word from the sieve buffer, marks each composite bit one by one by a code generated modulo marking pattern, and when all bits in a given word have been marked, commits the word back to the original location is the deliverable sieve buffer.

I believe that this is slower than other languages due to lack of optimization of the pattern selection process in these last techniques where it is not producing a computed goto jump table so I have emulated this capability with a Look Up Table (LUT) of anonymous functions that implement the "branches".  This is still likely a little slower than using a computed jump table in other languages, but improved the speed about 15 percent or more here as computer to using a sequential `match`.

## Author

W. Gordon Goodsman - GordonBGood
