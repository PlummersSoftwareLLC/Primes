![Algorithm](https://img.shields.io/badge/Algorithm-wheel-blue) ![Faithful](https://img.shields.io/badge/Faithful-yes-brightgreen) ![Parallel](https://img.shields.io/badge/Parallel-no-lightgrey) ![Bits](https://img.shields.io/badge/Bits-1-blueviolet)

### solution_1 — CTFE (Compile-Time Function Evaluation)

The entire Sieve of Eratosthenes runs at compile time via Zeta's `comptime` keyword. Runtime is a tight counter loop with periodic clock checks.

**19.1 billion passes/5s** — sieve evaluated once at compile time; binary prints constant 78,498 in a timed loop.

Tested on: Intel i9-13900H (Raptor Lake), 20 cores, WSL2 on Windows 11, Docker with ubuntu:24.04.
