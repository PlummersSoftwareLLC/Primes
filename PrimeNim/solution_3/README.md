# Nim solution #3 by GordonBGood

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Description

Although there is little point to a multi-threaded solution in showing which language is fastest for any of the languages as they will only show the effect of CPU throttling due to increased power usage for multiple cores and the effect of sharing resources, especially "Hyper-Threading" (HT)/""Simultaneous Multi Threading" (SMT) in sharing threads using common core execution unit resources and will be consistent in ratio to single threaded uses across languages, to be competitive a multi-threaded solution is provided.  Since for the metric of work done per thread for HT/SMT threads when all available threads are used drops by almost a factor of two plus the thermal throttling factor, some implementations have used less than the maximum number of threads to gain an apparent advantage in the multi-threading leaderboard, with one precident example using 4 threads and some forcing 16 threads in order to gain an advantage in the main test machine which has 32 threads on 16 cores using HT/SMT.  This seems objectionable as it tailors the test to this specific CPU and this implementation uses four threads, which should be available for all test machines.  This will provide an advantage on the 16 core test machine in less thermal throttling and less sharing of compute engine resources, but it will be no more than the advantage of the other accepted implementation using four thread.  As implied above, the multi-threading contest ruls should really be modified that all available threads must be used for a "maximum total work done" implementation.

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
GordonBGood_bittwiddle;8436;5.000193184;1;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8;12126;5.00009495;1;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8block-16K;15331;5.000233742;1;algorithm=base,faithful=yes,bits=1
GordonBGood_extreme;17846;5.000123127;1;algorithm=base,faithful=yes,bits=1
GordonBGood_extreme-hybrid;43711;5.000105359;1;algorithm=base,faithful=yes,bits=1
GordonBGood_bittwiddle;31778;5.006587674;4;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8;45603;5.003502314;4;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8block-16K;52680;5.00327773;4;algorithm=base,faithful=yes,bits=1
GordonBGood_extreme;67359;5.005868927;4;algorithm=base,faithful=yes,bits=1
GordonBGood_extreme-hybrid;163564;5.002152956;4;algorithm=base,faithful=yes,bits=1
```

## Benchmarks

Running locally on my Intel SkyLake i5-6500 at 3.6 GHz when single threaded, I get some astounding numbers:

```
GordonBGood_bittwiddle;8436;5.000193184;1;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8;12126;5.00009495;1;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8block-16K;15331;5.000233742;1;algorithm=base,faithful=yes,bits=1
GordonBGood_extreme;17846;5.000123127;1;algorithm=base,faithful=yes,bits=1
GordonBGood_extreme-hybrid;43711;5.000105359;1;algorithm=base,faithful=yes,bits=1
GordonBGood_bittwiddle;31778;5.006587674;4;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8;45603;5.003502314;4;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8block-16K;52680;5.00327773;4;algorithm=base,faithful=yes,bits=1
GordonBGood_extreme;67359;5.005868927;4;algorithm=base,faithful=yes,bits=1
GordonBGood_extreme-hybrid;163564;5.002152956;4;algorithm=base,faithful=yes,bits=1
```
Which matches the results when run with Docker on the same machine as follows:

```
                                                                Single-threaded                                                                 
┌───────┬────────────────┬──────────┬──────────────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                        │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼──────────────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ nim            │ 3        │ GordonBGood_extreme-hybrid   │ 42971  │ 5.00002  │    1    │   base    │   yes    │ 1    │  8594.17189   │
│   2   │ nim            │ 3        │ GordonBGood_extreme          │ 18030  │ 5.00009  │    1    │   base    │   yes    │ 1    │  3605.93329   │
│   3   │ nim            │ 3        │ GordonBGood_stride8block-16K │ 15434  │ 5.00030  │    1    │   base    │   yes    │ 1    │  3086.61194   │
│   4   │ nim            │ 3        │ GordonBGood_stride8          │ 12074  │ 5.00028  │    1    │   base    │   yes    │ 1    │  2414.66308   │
│   5   │ nim            │ 3        │ GordonBGood_bittwiddle       │  8469  │ 5.00029  │    1    │   base    │   yes    │ 1    │  1693.70180   │
└───────┴────────────────┴──────────┴──────────────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
                                                                 Multi-threaded                                                                 
┌───────┬────────────────┬──────────┬──────────────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                        │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼──────────────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ nim            │ 3        │ GordonBGood_extreme-hybrid   │ 161461 │ 5.00080  │    4    │   base    │   yes    │ 1    │  8071.76310   │
│   2   │ nim            │ 3        │ GordonBGood_extreme          │ 67586  │ 5.00086  │    4    │   base    │   yes    │ 1    │  3378.71766   │
│   3   │ nim            │ 3        │ GordonBGood_stride8block-16K │ 52922  │ 5.00060  │    4    │   base    │   yes    │ 1    │  2645.78250   │
│   4   │ nim            │ 3        │ GordonBGood_stride8          │ 45735  │ 5.00106  │    4    │   base    │   yes    │ 1    │  2286.26753   │
│   5   │ nim            │ 3        │ GordonBGood_bittwiddle       │ 31845  │ 5.00113  │    4    │   base    │   yes    │ 1    │  1591.88924   │
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
