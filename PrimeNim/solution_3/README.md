# Nim implementation #3 by GordonBGood

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Description

This solution contains implentations for five different composite number representations culling/marking techniques that all conform to being of the Sieve of Erathosthen type as follows:
1. The simplest is just the bit twidding/bit-packed masking one, with one small improvement in using a Look Up Table (LUT) for the bit masking patterns by bit index rather than using shifting as this is slightly faster.
2. The second technique uses striding in eight inner loops using a fixed mask for all bytes in each inner loop, for eight loops, each with a different mask value.  When there is "cache thrashing" as here, this often improves the performance due to the different cache line order, and also improves the performance because the inner loops are simpler.
3. Improving on the above technique, the next strided technique improves on the above by operating over a given block size at a time and thus slightly reducing the amount of "cache thrashing".  For this Nim version, the improvement is marginal in uses "blocking" this way.
4. The fourth technique is that of "Extreme Loop Unrolling" where the eight "strided" loops are assembled into a single loop by recognizing the modulo marking patterns and using immediate values for the marking of each individual composite number bit; this can't use the block approach because it advances through the sieve buffer from start to finish.  This is implemented using the `unrollLoops` macro.
5. The final technique uses the above for base prime values above 64 but for base primes below this threshold, it gives the dense marking treatment where it reads in a 64-bit word from the sieve buffer, marks each composite bit one by one by a macro generated modulo marking pattern, and when all bits in a given word have been marked, the word is commited back to the original location in the deliverable sieve buffer.  This dense marking macro is the `dense_setbits` macro.

## Run

### Run locally

Nim is available via package managers under the popular systems. The following command should get you started:

```
nim c --gc:arc -d:danger -t:"-march=native" -d:lto -r primes.nim
```

### Docker

As per the usual minimal solution with just a minimal set of commands:

```
docker build -t primes .
docker run --rm primes
```

The Dockerfile uses the custom built `primeimages/nim:1.4.8` so it can run on both arm64 and amd64.

## Benchmarks

This version runs about the same speed run either on the Docker image or locally when run on the same machine.

## Output

The following is as run on an Intel SkyLake i5-6500 at 3.6 GHz (single-threaded):
```
GordonBGood_extreme_hybrid;44473;5.000062845;1;algorithm=base,faithful=yes,bits=1
```

## Benchmarks

Running locally on my Intel SkyLake i5-6500 at 3.6 GHz when single threaded, I get some astounding numbers:

```
Passes: 8549, Time: 5.00003205, Avg: 0.0005848674757281553, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
GordonBGood_bittwiddle;8549;5.00003205;1;algorithm=base,faithful=yes,bits=1
Passes: 12179, Time: 5.000142803, Avg: 0.0004105544628458823, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
GordonBGood_stride8;12179;5.000142803;1;algorithm=base,faithful=yes,bits=1
Passes: 15481, Time: 5.000130019, Avg: 0.0003229849505199923, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
GordonBGood_stride8block-16K;15481;5.000130019;1;algorithm=base,faithful=yes,bits=1
Passes: 18332, Time: 5.000006381, Avg: 0.0002727474569605062, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
GordonBGood_extreme;18332;5.000006381;1;algorithm=base,faithful=yes,bits=1
Passes: 44094, Time: 5.000062015, Avg: 0.0001133955190048533, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
GordonBGood_extreme-hybrid;44094;5.000062015;1;algorithm=base,faithful=yes,bits=1
```
Which matches the results when run with Docker on the same machine as follows:

```
                                                                Single-threaded                                                                 
┌───────┬────────────────┬──────────┬──────────────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                        │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼──────────────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ nim            │ 3        │ GordonBGood_extreme-hybrid   │ 43730  │ 5.00014  │    1    │   base    │   yes    │ 1    │  8745.75303   │
│   2   │ nim            │ 3        │ GordonBGood_extreme          │ 18115  │ 5.00016  │    1    │   base    │   yes    │ 1    │  3622.88491   │
│   3   │ nim            │ 3        │ GordonBGood_stride8block-16K │ 15389  │ 5.00014  │    1    │   base    │   yes    │ 1    │  3077.71376   │
│   4   │ nim            │ 3        │ GordonBGood_stride8          │ 12196  │ 5.00025  │    1    │   base    │   yes    │ 1    │  2439.07929   │
│   5   │ nim            │ 3        │ GordonBGood_bittwiddle       │  8480  │ 5.00022  │    1    │   base    │   yes    │ 1    │  1695.92701   │
└───────┴────────────────┴──────────┴──────────────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```

## Notes

This benchmark is written to be true to the "faithful to base algorithm" specification as follows:

1. It implements the odds-only bit-packed Sieve of Eratosthenes algorithm.
2. The output array contains at least one bit for each odd number in the sieved range as per the required algorithm.
3. The program marks as composite all odd composite representative bits that represent multiples of the odd base primes.
4. This implementation starts looking for base primes at three and scans only odd value indices up to the square root of the limit for prime base numbers as is allowed.
5. There is an outer loop that scans for instances of odd base prime values from three up to the index representing the square root of the limit, and with the body of this outer loop then marking all multiples of the found base prime value starting at the index representing the square of that base prime for all the multiples in the delivered sieving array as is also allowed.
6. The sieving buffer starts in zeroed state with composite number bit representatives marked as one when culled as is allowed.
7. There are approximately 810,000 culling/marking of composite representations just as for the original odds-only algorithm.

There are minor tweaks, but one reason that this implementation is so much faster than others, is "Extreme Loop Unrolling" using a Nim macro to generate the thousands of lines of code based on modulo patterns that result from stepping within a (even bits) byte array.  This technique is similar to the "striping" technique used by the fastest "faithful to base algorithm" Rust implementation which culls by eight simpler-than-"bit-twiddling"-per-bit constant mask value loops per base prime, but this implementation instead combines those eight "striping" loops into a single loop with eight culling operations per loop, with the macro calculating the necessary variations as to the immediate bit mask to be used for each cull in each of the patterns.  Thus, it is similar to the techniques that have already been accepted, just refined to the utmost.  One can see the generated code at compile time for the base prime/step value of base prime values that have a modulo eight value of one with culling starting a bit index of zero by uncommenting line 101 and compiling.

The other even more important reason this implementation is so fast is that it uses the same dense culling for small base prime values as in the fastest Chapel version, where when there are multiple bits per 64-bit word to be culled, the entire word is read into a variable `v`, the individual bits to be culled are marked by a modulo pattern mask each, and when all bits to be culled in the given word have been marked, the value `v` is committed back to the original location in the deliverable sieve buffer; this sequence is then repeated for the number of words in the repeating modulo pattern.  This is so fast for two reasons:  first, it reduces the average operation time to as little a one third of a CPU clock cycle per operations, and second, it improves cache associativity by cache lines as it advances linearly across the entire sieving buffer, just as the above extreme loop unrolling does, but now the extreme loop unrolling also advances by at least eight bytes at a time.  This dense marking technique is likely even more important than the extreme loop unrolling technique as many more of the operations ar dense than not.  The dense threshold has been set at base primes values up to 129, and again helps speed as compared to the Chapel version for which the dense base prime threshold is 19.  One can see the generated code at compile time for the base prime/step value of three by uncommenting line 173 and compiling.

In order to make it obvious that this implementation is "faithful to base", the `bitSeq`  bit array has been implemented as a custom type with all memory access operations done through custom functions/methods/operators; in particular the `setRange` function implements a marking starting at an index, to an index, stepping by an index which is a built-in ability in many languages.

This technique allows culling/marking operations to take an average of about 0.5 CPU clock cycles per marking operation with a modern high-efficiency CPU.

As common to all efficient SoE implementations, almost all of the expended time is spent in the composite number culling/marking.

## Author

I can be contacted as GordonBGood on the [Nim forum](https://forum.nim-lang.org/) - W. Gordon Goodsman
