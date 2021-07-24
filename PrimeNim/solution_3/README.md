# Nim implementation #3

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Run

### Run locally

Nim is available via package manager under the popular systems. The following command should get you started:

```
nim c --gc:arc -d:danger -d:danger -t:"-march=native" -d:lto -r primes.nim
```

### Docker

As per usual minimal, just a minimal set of commands:

```
docker build -t primes .
docker run --rm primes
```

Uses the official Alpine images, which for now have no support for ARM64.

## Benchmarks

Gains around 6.75 times the performance from the Nim `solution_2` with the flags as above when run with Docker; for some reason the "solution_1" and "solution_2" versions run about a factor of two faster on a local machine than on the Docker image, whereas this version runs the same speed on either.  Those older versions may be slow because there is too much procedure calling, parameter passing, and especially conditional statements in the culling loop, meaning that the abilities of the branch prediction will play a major role, where this version has hardly any of these things.

## Output
```
GordonBGood_1of2;19183;5.000058949;1;algorithm=base,faithful=yes,bits=1
```

## Benchmarks

Running locally on my Intel SkyLake i5-6500 at 3.6 GHz when single threaded, I get some astounding numbers:

```
Passes: 18913, Time: 5.000210011, Avg: 0.0002643795278908687, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 19162, Time: 5.00014818, Avg: 0.0002609408297672477, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 19179, Time: 5.000080514, Avg: 0.0002607060072996507, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 19165, Time: 5.000035447, Avg: 0.0002608941010696582, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```
Which matches the results when run with Docker on the same machine.

## Notes

This benchmark is written to be true to the "faithful to base algorithm" specification as follows:

1. It implements the odds-only bit-packed Sieve of Eratosthenes algorithm.
2. The output array contains at least one bit for each number in the sieved range as per the original algorithm; this is a waste of half of the memory for all the bits that represent even numbers that aren't used in sieving.
3. Due to the unused bits, the culling operations span is increased by a factor of two times the base prime number in order to skip over the unused bits representing the even numbers.
4. It starts looking for base primes at three and scans only odd value indices up to the square root of the limit for prime base numbers as allowed.
5. The base primes and the square of the base primes cull starting index is determined in a first pass with the results stored in intermediate arrays, followed by the culling operations starting at the square of each base prime as is also allowed.
6. The sieving buffer starts in zeroed state with composite number bit representatives marked as one when culled as is allowed.
7. The composite culling by multiples of the base primes in sequence is the last phase of the sieving operation, which is an allowed option.
8. Something that differs from other common implementations is that the culling is done by CPU L1 cache sized pages at a time, with the resulting sieved pages copied into the deliverable array; this is a minor optimization that doesn't seem to conflict with the base specification.

There are minor tweaks, but the major reason that this implementation is so much faster than others, is "Extreme Loop Unrolling" using a Nim macro to generate the over a thousand lines of code based on modulo patterns that result from culling by odd base primes within a byte array with an even number of bits per byte, which optimization is enabled by culling within one CPU L1 cache buffer at a time to avoid "cache thrashing".  This technique is similar to the "striping" technique used by the fastest "faithful to base algorithm" Rust implementation which culls by eight simpler-than-"bit-twiddling"-per-bit constant-mask-value loops per base prime, but this implementation instead combines those eight "striping" loops into a single loop with eight culling operations within each loop, with the macro calculating the necessary variations as to the immediate bit masks to be used for each cull in each of the patterns.  Thus, it is similar to the techniques that have already been accepted, just refined to the utmost.

This technique, when used with a CPU L1 cache-sized buffer, allows culling operations to take an average of just over one CPU clock cycle per read/modify/write `or` machine instruction used to mark the composites with a modern high-efficiency CPU, but there is about a ten per cent overhead due to the need to copy the sieved partial array into the deliverable array.

As common to all efficient SoE implementations, almost all of the expended time is still spent in the composite number culling loops.

Along with the use of page-segmentation, the technique described above forms the basis of the ability to odds-only single-threaded sieve (as here) to a range of a hundred billion (10^11) in about a minute on a modern CPU, reduced by a factor of about four with "Maximal Wheel Factorization", and yet further reduced by the number of effective "real" (not HT/SMT) threads for "industrial strength" sieving.

## Author

I can be contacted as GordonBGood on the [Nim forum](https://forum.nim-lang.org/) - W. Gordon Goodsman
