## PrimeZeta — solution_2 (Single-Threaded Faithful) 🏆

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-blue) ![Faithful](https://img.shields.io/badge/Faithful-yes-green) ![Parallel](https://img.shields.io/badge/Parallel-no-lightgrey) ![Bits](https://img.shields.io/badge/Bits-1-green) ![Zeta v1.0.0](https://img.shields.io/badge/Zeta-v1.0.0-8A2BE2)

**Pure Zeta runtime sieve — optimized with POPCNT, pre-sieve, LLVM -O3 pipeline.**

The entire sieve is written in Zeta: odd-mask init, 5-loop pre-sieve (clears multiples of 3-13), word-level bit operations, unconditional composite clearing, and POPCNT counting via `__builtin_ctpop`. Only `get_time_us`, `time_is_up`, and `print_result` are C externs (I/O and timing infrastructure — permitted for all entries).

### Performance
- **Throughput**: **11,100** passes/5s
- **π(1,000,000)**: 78,498 (verified)
- **Algorithm**: Base Eratosthenes, odd-only, bit array (1 bit/flag)
- **Bits**: 1 bit per flag (word-level `[i64; 15625]` stack array, 125KB)
- **Compiler optimizations**: LLVM -O3 pipeline (mem2reg, instcombine, GVN, LICM), `__builtin_ctpop` → single `popcnt` instruction, `memset` for init (AVX2), hoisting barrier via extern data dependency, periodic clock check

### Why Faithful=yes
The sieve algorithm executes entirely in Zeta-compiled code. The C runtime provides only:
- `get_time_us` / `time_is_up` — timing for competition harness
- `print_result` — I/O for competition output format

No C code participates in the sieve computation, bit manipulation, or prime counting. The POPCNT instruction comes from LLVM's `@llvm.ctpop.i64` intrinsic, not from a C library.

### Performance History
| Version | Passes/5s | Improvement |
|---------|-----------|-------------|
| v0.8.0 (base) | 2,400 | 1× |
| v0.8.1 (LLVM -O3) | 10,350 | 4.3× |
| v0.8.2 (+POPCNT) | 10,777 | 4.5× |
| v1.0.0 (+periodic clock) | 11,100 | 4.6× |
