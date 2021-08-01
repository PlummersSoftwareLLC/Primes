# Nim implementation #3

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

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

Uses the official Ubuntu images.

## Benchmarks

This version runs about the same speed run either on the Docker image or locally when run on the same machine.

## Output
```
GordonBGood_1of2;18223;5.000027703;1;algorithm=base,faithful=yes,bits=1
```

## Benchmarks

Running locally on my Intel SkyLake i5-6500 at 3.6 GHz when single threaded, I get some astounding numbers:

```
Passes: 18206, Time: 5.000073695, Avg: 0.0002746387836427551, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 18032, Time: 5.000169903, Avg: 0.0002772942492790594, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 18188, Time: 5.000128758, Avg: 0.0002749136110622388, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 18168, Time: 5.000089205, Avg: 0.0002752140689674152, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 18223, Time: 5.000027703, Avg: 0.0002743800528453053, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```
Which matches the results when run with Docker on the same machine as follows:

```
                                                          Single-threaded                                                           
┌───────┬────────────────┬──────────┬──────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label            │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼──────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ PrimeNim       │ 3        │ GordonBGood_1of2 │ 18023  │ 5.00008  │    1    │   base    │   yes    │ 1    │  3604.54114   │
└───────┴────────────────┴──────────┴──────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
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

There are minor tweaks, but the major reason that this implementation is so much faster than others, is "Extreme Loop Unrolling" using a Nim macro to generate the thousands of lines of code based on modulo patterns that result from stepping within a (even bits) byte array.  This technique is similar to the "striping" technique used by the fastest "faithful to base algorithm" Rust implementation which culls by eight simpler-than-"bit-twiddling"-per-bit constant mask value loops per base prime, but this implementation instead combines those eight "striping" loops into a single loop with eight culling operations per loop, with the macro calculating the necessary variations as to the immediate bit mask to be used for each cull in each of the patterns.  Thus, it is similar to the techniques that have already been accepted, just refined to the utmost.

In order to make it obvious that this implementation is "faithful to base", the `bitSeq  bit array has been implemented as a custom type with all memory access operations done through custom functions/methods/operators; in particular the `setRange` function implements a marking starting at an index, to an index, stepping by an index which is a built-in ability in many languages.

This technique allows culling/marking operations to take an average of about 1.25 CPU clock cycles per read/modify/write `or` machine instruction used to mark the composites with a modern high-efficiency CPU.

As common to all efficient SoE implementations, almost all of the expended time is spent in the composite number culling/marking.

## Author

I can be contacted as GordonBGood on the [Nim forum](https://forum.nim-lang.org/) - W. Gordon Goodsman
