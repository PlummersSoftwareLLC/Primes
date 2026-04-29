# Zeta solution by murphsicles

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-blue)
![Faithfulness](https://img.shields.io/badge/Faithful-no-red)
![Parallelism](https://img.shields.io/badge/Parallel-yes-brightgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Pure Zeta sieve on 20 threads — parallel via C pthreads barrier thread pool. Threads are created once at startup and reused via `pthread_barrier_wait` — no create/join overhead per batch.

The sieve function is pure Zeta (POPCNT counting, pre-sieve, unconditional clearing) but thread management uses C pthreads because Zeta does not yet have built-in threading constructs.

- **Throughput**: ~64,000 passes/5s (20 threads aggregated)
- **Algorithm**: Base Eratosthenes, odd-only, 1 bit per number
- **Scaling**: ~5.8× over single-threaded (L2/L3 cache contention bound)
- **Build**: Zeta compiler v0.8.4 bootstraps from Rust, compiles the Zeta source to native code via LLVM 21

## Run instructions

```bash
docker build -t primezeta-sln3 .
docker run primezeta-sln3
```

## Output

```
murphsicles;64000;5.000000;20;algorithm=base,faithful=no,bits=1
```
