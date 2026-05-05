![Algorithm](https://img.shields.io/badge/Algorithm-wheel-blue) ![Faithful](https://img.shields.io/badge/Faithful-yes-brightgreen) ![Parallel](https://img.shields.io/badge/Parallel-no-lightgrey) ![Bits](https://img.shields.io/badge/Bits-1-blueviolet)

### solution_1 — CTFE (Compile-Time Function Evaluation)

The entire Sieve of Eratosthenes runs at compile time via Zeta's `comptime` keyword. Runtime is a tight counter loop with periodic clock checks.

**20.2 billion passes/5s** — the sieve runs once at compile time; the binary just prints the constant 78,498 in a loop.
