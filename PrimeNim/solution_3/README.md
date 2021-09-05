# Nim implementation #3 by GordonBGood

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
Passes: 44319, Time: 5.000079148, Avg: 0.0001128202158893477, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 43944, Time: 5.000009589, Avg: 0.000113781394251775, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 44504, Time: 5.000041183, Avg: 0.0001123503771121697, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 44456, Time: 5.000045385, Avg: 0.00011247177850009, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 43888, Time: 5.000057349, Avg: 0.0001139276647147284, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```
Which matches the results when run with Docker on the same machine as follows:

```
                                                               Single-threaded                                                                
┌───────┬────────────────┬──────────┬────────────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                      │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼────────────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ nim            │ 3        │ GordonBGood_extreme_hybrid │ 44189  │ 5.00004  │    1    │   base    │   yes    │ 1    │  8837.73123   │
└───────┴────────────────┴──────────┴────────────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
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

There are minor tweaks, but one reason that this implementation is so much faster than others, is "Extreme Loop Unrolling" using a Nim macro to generate the thousands of lines of code based on modulo patterns that result from stepping within a (even bits) byte array.  This technique is similar to the "striping" technique used by the fastest "faithful to base algorithm" Rust implementation which culls by eight simpler-than-"bit-twiddling"-per-bit constant mask value loops per base prime, but this implementation instead combines those eight "striping" loops into a single loop with eight culling operations per loop, with the macro calculating the necessary variations as to the immediate bit mask to be used for each cull in each of the patterns.  Thus, it is similar to the techniques that have already been accepted, just refined to the utmost.

The other even more important reason this implementation is so fast is that it uses the same dense culling for small base prime values as in the fastest Chapel version, where when there are multiple bits per 64-bit word to be culled, the entire word is read into a variable `v`, the individual bits to be culled are marked by a modulo pattern mask each, and when all bits to be culled in the given word have been marked, the value `v` is committed back to the original location in the deliverable sieve buffer; this sequence is then repeated for the number of words in the repeating modulo pattern.  This is so fast for two reasons:  first, it reduces the average operation time to as little a one third of a CPU clock cycle per operations, and second, it improves cache associativity by cache lines as it advances linearly across the entire sieving buffer, just as the above extreme loop unrolling does, but now the extreme loop unrolling also advances by at least eight bytes at a time.  This dense marking technique is likely even more important than the extreme loop unrolling technique as many more of the operations ar dense than not.  The dense threshold has been set at base primes values up to 129, and again helps speed as compared to the Chapel version for which the dense base prime threshold is 19.  One can see the generated code at compile time for the base prime/step value of three by uncommenting line 162 and compiling.

In order to make it obvious that this implementation is "faithful to base", the `bitSeq`  bit array has been implemented as a custom type with all memory access operations done through custom functions/methods/operators; in particular the `setRange` function implements a marking starting at an index, to an index, stepping by an index which is a built-in ability in many languages.

This technique allows culling/marking operations to take an average of about 0.5 CPU clock cycles per marking operation with a modern high-efficiency CPU.

As common to all efficient SoE implementations, almost all of the expended time is spent in the composite number culling/marking.

## Author

I can be contacted as GordonBGood on the [Nim forum](https://forum.nim-lang.org/) - W. Gordon Goodsman
