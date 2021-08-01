# Rust solution by @SergioCKS

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

> A Rust implementation of a wheel-based algorithm for generating primes.

More details and references about the algorithm can be found in the code documentation.

## Instructions

### Build with Cargo

``cargo build --release``

This command produces the binary `target/release/rust-wheel-sieve`.

### Build Docker image

``docker build . -t rust-wheel-sieve:latest``

During build, the correctness tests will be run.

### Run with cargo

``cargo run --release``

This command builds the optimized binary `target/release/rust-wheel-sieve` and runs it.

### Run Docker image

``docker run --rm rust-wheel-sieve:latest``

### Run tests with cargo

``cargo test``

This command will run all 8 correctness tests.

## Output

Output obtained on an `AMD Ryzen 9 4900HS` (no Docker):

```text
Generating primes up to 1000000...
Number of threads: 1.
Run duration (in seconds): 5

Results:

sergiocks;18171;5.0001991;1;algorithm=wheel,faithful=yes,bits=8


Generating primes up to 1000000...
Number of threads: 16.
Run duration (in seconds): 5

Results:

sergiocks;120653;5.0016166;16;algorithm=wheel,faithful=yes,bits=8
```

Same setup with Docker:

```text

Generating primes up to 1000000...
Number of threads: 1.
Run duration (in seconds): 5

Results:

sergiocks;18805;5;1;algorithm=wheel,faithful=yes,bits=8


Generating primes up to 1000000...
Number of threads: 16.
Run duration (in seconds): 5

Results:

sergiocks;112726;5;16;algorithm=wheel,faithful=yes,bits=8
```

## Algorithm Description

The algorithm used is based on [Pritchard's wheel sieve](https://www.semanticscholar.org/paper/Explaining-the-wheel-sieve-Pritchard/675801e3b109441cb59e6a368181cfc4a6f84519).

In the context of this algorithm, the `n`th *wheel* is the set of positive integers between 1 and the product of the first `n` primes that are coprime to all first `n` primes. For example the first wheel is `{1}`, the second is `{1,5}`, the third is `{1,7,11,13,17,19,23,27,29}` and so on.

If we have the `n`th wheel and `p` is the `n+1` smallest prime, we can construct the `n+1` wheel by *rolling* the `n`th wheel `p` times and then sieving out multiples of `p`. Rolling the wheel means repeating the pattern by shifting the wheel.

For example, to construct the second wheel from the first one, we roll the first wheel `{1}` three times to obtain `{1,3,5}` and then sieve the multiples of 3 to obtain the next wheel `{1,5}`. Then, to obtain the next wheel, we roll this wheel five times to obtain `{1,5,7,11,13,17,21,23,27,29}` and then sieve out the multiples of 5 to obtain the third wheel.

We continue generating larger and larger wheels until the size of the next wheel (the product of the base primes) reaches the desired limit. For a limit of one million, for exaple, the algorithm will compute a truncated version of the 8th wheel (of size `2*3*5*7*11*13*17*19 = 9,699,690`). This wheel is guaranteed to not contain multiples of the first 8 primes, but may still contain multiples of larger primes.

To sieve out the remaining possible composites in the wheel, it is necessary to continue sieving larger primes using a regular sieve.

## Optimization Possibilities

The following insights may lead to further optimizations, although the complexity of implementation may not be worth the gains.

### Algorithm based optimizations

1. **Exploit wheel symmetry:**
   It turns out that the generated wheels are symmetric
   in the sense that the `i`th element is the same as the `n-i`th element (where `n` is the size of the wheel).
   
   Right now, the implementation doesn't take this into consideration.
   
   It may be possible to reduce some sieve passes by sieving only half of the wheel and copying the sieved half.
   
2. **Reduce sieve passes while generating wheels:**
   To generate the next wheel, besides "rolling the wheel", it is necessary to sieve multiples of the next prime,
   but only once for each element in the previous wheel.
   
   Currently, the implementation sieves all multiples, as the cost
   of checking if the multiple is part of the previous wheel already surpasses the benefits.
   
   It may be possible to reduce some sieve passes by keeping track of (and updating) the necessary multiples and only applying those.
