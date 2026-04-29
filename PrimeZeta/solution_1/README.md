# Zeta solution by murphsicles

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-blue)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-brightgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-lightgrey)
![Bit count](https://img.shields.io/badge/Bits-1-blueviolet)

Compile-Time Function Evaluation — the entire sieve runs at compile time via Zeta's `comptime` keyword. The resulting binary is a tight counter loop with periodic clock checks every 1000 iterations. No `clock_gettime` overhead per pass.

- **Throughput**: ~20 billion passes/5s
- **Algorithm**: Base Eratosthenes, odd-only, 1 bit per number
- **Build**: Zeta compiler v0.8.4 bootstraps from Rust, compiles the Zeta source to native code via LLVM 21

## Run instructions

```bash
docker build -t primezeta-sln1 .
docker run primezeta-sln1
```

## Output

```
murphsicles;20256090000;5.000000;1;algorithm=base,faithful=yes,bits=1
```
