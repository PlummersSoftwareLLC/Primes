# Zeta solution by murphsicles

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-blue)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-lightgrey)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Pure Zeta runtime sieve — the entire algorithm is written in Zeta and compiled to native code via LLVM 21. Uses POPCNT counting (`__builtin_ctpop` → `popcnt` instruction), 5-loop pre-sieve for small primes, and unconditional composite clearing.

Only `get_time_us`, `time_is_up`, and `print_result` are C externs — I/O and timing infrastructure permitted for all entries.

- **Throughput**: ~11,000 passes/5s
- **Algorithm**: Base Eratosthenes, odd-only, 1 bit per number
- **Build**: Zeta compiler v0.8.4 bootstraps from Rust, compiles the Zeta source to native code via LLVM 21
- **Compiler optimizations**: LLVM -O3 pipeline (mem2reg, instcombine, GVN, LICM), `__builtin_ctpop` → single `popcnt` instruction, `memset` for init (AVX2), periodic clock check

## Run instructions

```bash
docker build -t primezeta-sln2 .
docker run primezeta-sln2
```

## Output

```
murphsicles;11100;5.000000;1;algorithm=base,faithful=yes,bits=1
```
