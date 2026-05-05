## PrimeZeta — solution_3 (Multi-Threaded Parallel) 🏆

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-blue) ![Faithful](https://img.shields.io/badge/Faithful-no-red) ![Parallel](https://img.shields.io/badge/Parallel-yes-brightgreen) ![Bits](https://img.shields.io/badge/Bits-1-green) ![Zeta v1.0.0](https://img.shields.io/badge/Zeta-v1.0.0-8A2BE2)

**Pure Zeta sieve on 20 threads — barrier-based thread pool.**

Threads are created ONCE at startup and reused via `pthread_barrier_wait`. No `pthread_create`/`pthread_join` overhead per batch. Each thread runs the same optimized Zeta sieve independently — separate stack-allocated bit array, no shared state, no locks.

Optimizations applied: `__builtin_ctpop` (POPCNT instruction), LLVM -O3 pass pipeline, pre-sieve clearing, unconditional composite clearing, barrier-based thread pool.

### Performance
- **Throughput**: **64,000** passes/5s (20 threads aggregated)
- **π(1,000,000)**: 78,498
- **Algorithm**: Base Eratosthenes, odd-only, bit array
- **Scaling**: ~5.8× over single-threaded (cache contention bound)

### Why Faithful=no
The sieve function itself is pure Zeta, but thread pool management uses C pthreads barriers. Zeta does not have built-in threading constructs (yet).

### Compiler
Zeta v1.0.0 — LLVM -O3 pipeline, `__builtin_ctpop` intrinsic, POPCNT instruction, AVX2 auto-vectorization.
