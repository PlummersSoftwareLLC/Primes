# Rust solution by @bonnhatnguyen and @CokieMiner

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A collection of high-performance, 1-bit Wheel 8-of-30 (primes 2, 3, 5) Sieves of Eratosthenes written in pure Rust with zero external dependencies.

## Implementations

### `rust-wheel8-sieve` by @bonnhatnguyen

1. **Wheel 8-of-30 Factorization:** Only candidates coprime to 2, 3, and 5 (residues 1, 7, 11, 13, 17, 19, 23, 29) are tracked and visited. All multiples of 2, 3, and 5 are completely eliminated upfront.
2. **1-Bit Flag Storage:** Only odd numbers are addressed, packing 1 bit per candidate flag in 64-bit words (`u64`). For $N = 1,000,000$, the working set is exactly 7,813 `u64` words (~61.0 KiB / 62,504 bytes), reducing the memory footprint by 8x compared to byte-flag implementations (such as `solution_7` at ~500 KB) and fitting comfortably in modern CPU L2 cache for exceptional memory bandwidth throughput.
3. **8-Step Unrolled Cycle:** When sieving multiples of a prime, the algorithm unrolls a full 8-step wheel stride cycle (`factor * [3, 2, 1, 2, 1, 2, 3, 1]`, summing to $15 \times factor$ in halved coordinates). All inner loop branches and modulo checks are completely eliminated.
4. **Hardware Acceleration:** Native bit manipulation and automatic thread scaling via standard library `available_parallelism`.
5. **Faithful Compliance:** Sieve state is fully encapsulated in `PrimeSieve`, allocated dynamically at runtime on each pass, without precomputed prime tables.

### `cokieminer-sieve` by @CokieMiner

1. **Residue-Packed Bitmap Layout:** Only the eight residues coprime to 30 are given flags, packed exactly 8 bits per 30 numbers into one byte per group (33,334 bytes at limit 1,000,000). Multiples of 2, 3, and 5 have no flags.
2. **Runtime Seed Pattern Generation:** At runtime on every pass, periodic seed patterns covering primes through 89 are generated directly in the allocated buffer.
3. **Hole-Skipping Marking Streams:** Remaining factors are found in the sieve using fixed-mask marking streams that omit flags already marked by the prime 7 seed pattern, eliminating redundant cache writes.
4. **Multithreaded Scaling:** Supports independent workers using `std::thread`, with private sieves and memory allocations per worker.

## Run instructions

### Native Rust (Cargo)

```bash
cargo test
cargo run --release --bin rust-wheel8-sieve
cargo run --release --bin cokieminer-sieve
cargo run --release --bin cokieminer-sieve -- --threads auto
```

Or run all implementations in sequence:

```bash
cargo build --release
./run.sh
```

### Docker

```bash
docker build -t rust-wheel8-sieve .
docker run --rm rust-wheel8-sieve
```

## Output

### `rust-wheel8-sieve` by @bonnhatnguyen
Executed on an Intel Core i7-12700H (14 cores, 20 logical threads) running Windows 11 / Rust 1.85:

```text
bonnhatnguyen-rust-wheel8;36798;5.000109;1;algorithm=wheel,faithful=yes,bits=1
bonnhatnguyen-rust-wheel8-par;225881;5.007622;20;algorithm=wheel,faithful=yes,bits=1
```

### `cokieminer-sieve` by @CokieMiner
Executed on an AMD Ryzen AI 7 350 (16 threads) running Linux / Rust 1.85:

```text
CokieMiner_rust_w30;199391;5.000019767;1;algorithm=wheel,faithful=yes,bits=1
CokieMiner_rust_w30;1359830;5.000430594;16;algorithm=wheel,faithful=yes,bits=1
```

## License

- The files in `cokieminer-sieve/` are licensed under the MIT License (see [`cokieminer-sieve/LICENSE`](cokieminer-sieve/LICENSE)).
- All other code in this solution is licensed under the repository's BSD-3-Clause license.
