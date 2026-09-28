# C++ solution by ndt0208

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A high-performance, faithful, 1-bit Sieve of Eratosthenes implementing a 8-of-30 Wheel Factorization algorithm in modern C++ (C++17).

## Key Characteristics & Optimizations

1. **Wheel 8-of-30 Factorization**:
   - Skips multiples of 2, 3, and 5 automatically.
   - For every 30 integers, exactly 8 coprime candidates remain: `[1, 7, 11, 13, 17, 19, 23, 29]`.
   - The cycle consists of 8 halved step strides: `[3, 2, 1, 2, 1, 2, 3, 1] * factor` (sum = `15 * factor`).

2. **Register Allocation & Unrolled Inner Loop**:
   - The 8 stride offsets fit completely inside CPU registers (`rax`, `rbx`, `rcx`, `rdx`, `r8`..`r11`).
   - The inner clearing loop is unrolled by the full 8-step cycle, eliminating loop control overhead, branch mispredictions, and lookup table dereferences during the sieve traversal.

3. **L1 Cache Residency**:
   - Storing only odd numbers at 1 bit per odd integer requires only `(1,000,000 / 2) / 8 = 62,500` bits ≈ 7,813 bytes in standard 1-bit odds representation, or 33,334 bytes for direct half-integer mapping.
   - The buffer occupies ~32.55 KB, fitting entirely inside the L1 Data Cache (32 KB - 48 KB on modern x86/ARM processors), eliminating L2/L3 cache misses during marking.
   - Buffer memory is 64-byte aligned to cache line boundaries via `posix_memalign` / `_aligned_malloc`.

4. **Strict Faithfulness**:
   - Self-contained `PrimeSieve` class encapsulates the entire sieve state.
   - Sieve buffer is dynamically allocated on each pass at runtime according to the requested size.
   - Validates correct prime count (78,498 primes under 1,000,000).

## Run Instructions

### Docker

Build and run using Docker:

```bash
docker build -t primecpp-sol6 .
docker run --rm primecpp-sol6
```

### Native (Linux / macOS)

```bash
./run.sh
```

### Native (Windows)

```cmd
.\run.cmd
```

## Output

Benchmarked on Intel Core i7-12700H (Docker Ubuntu 22.04 container):

```text
ndt0208-cpp-wheel8;43363;5.00002;1;algorithm=wheel,faithful=yes,bits=1
ndt0208-cpp-wheel8-par;215758;5.00454;20;algorithm=wheel,faithful=yes,bits=1
```
