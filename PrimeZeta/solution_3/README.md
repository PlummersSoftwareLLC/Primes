![Algorithm](https://img.shields.io/badge/Algorithm-wheel-blue) ![Faithful](https://img.shields.io/badge/Faithful-no-red) ![Parallel](https://img.shields.io/badge/Parallel-yes-brightgreen) ![Bits](https://img.shields.io/badge/Bits-1-green)

### solution_3 — Multi-Threaded Parallel

20 threads running the same Zeta sieve independently via a barrier-based thread pool. No shared state, no locks. Thread pool management uses C pthreads; the sieve itself is pure Zeta.

**64,000 passes/5s** (20 threads aggregated).
