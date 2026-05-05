![Algorithm](https://img.shields.io/badge/Algorithm-wheel-blue) ![Faithful](https://img.shields.io/badge/Faithful-yes-green) ![Parallel](https://img.shields.io/badge/Parallel-no-lightgrey) ![Bits](https://img.shields.io/badge/Bits-1-green)

### solution_2 — Single-Threaded Faithful

Pure Zeta runtime sieve. Word-level bit operations, POPCNT via `__builtin_ctpop`, odd-only 30030-wheel factorization. All algorithm code is Zeta; only I/O and timing are C externs.

**11,100 passes/5s** — LLVM -O3 with mem2reg, instcombine, GVN, LICM.
