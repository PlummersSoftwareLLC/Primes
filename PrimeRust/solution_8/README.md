# Rust solution by @bonnhatnguyen

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A high-performance, 1-bit Wheel 8-of-30 (primes 2, 3, 5) Sieve of Eratosthenes written in pure Rust with zero external dependencies.

## Key Features & Optimizations

1. **Wheel 8-of-30 Factorization:** Only candidates coprime to 2, 3, and 5 (residues 1, 7, 11, 13, 17, 19, 23, 29) are tracked and visited. All multiples of 2, 3, and 5 are completely eliminated upfront.
2. **1-Bit Flag Storage:** Only odd numbers are addressed, packing 1 bit per candidate flag in 64-bit words (`u64`). For $N = 1,000,000$, the working set is exactly 7,813 `u64` words (~61.0 KiB / 62,504 bytes), reducing the memory footprint by 8x compared to byte-flag implementations (such as `solution_7` at ~500 KB) and fitting comfortably in modern CPU L2 cache for exceptional memory bandwidth throughput.
3. **8-Step Unrolled Cycle:** When sieving multiples of a prime, the algorithm unrolls a full 8-step wheel stride cycle (`factor * [3, 2, 1, 2, 1, 2, 3, 1]`, summing to $15 \times factor$ in halved coordinates). All inner loop branches and modulo checks are completely eliminated.
4. **Hardware Acceleration:** Native bit manipulation, popcount instructions for instantaneous verification, and automatic thread scaling via standard library `available_parallelism`.
5. **Faithful Compliance:** Sieve state is fully encapsulated in `PrimeSieve`, allocated dynamically at runtime on each pass, without precomputed prime tables.

## Run instructions

### Native Rust (Cargo)

```bash
cargo test
cargo run --release
```

### Docker

```bash
docker build -t rust-wheel8-sieve .
docker run --rm rust-wheel8-sieve
```

## Output

Executed on an Intel Core i7-12700H (14 cores, 20 logical threads) running Windows 11 / Rust 1.85:

```text
bonnhatnguyen-rust-wheel8;36798;5.000109;1;algorithm=wheel,faithful=yes,bits=1
bonnhatnguyen-rust-wheel8-par;225881;5.007622;20;algorithm=wheel,faithful=yes,bits=1
```
