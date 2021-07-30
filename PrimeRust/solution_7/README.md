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
