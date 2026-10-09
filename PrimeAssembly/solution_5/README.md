# x86-64 AVX2 Modulo-30 Wheel Assembly Solution by TACITVS

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A handwritten x86-64 assembly implementation of the Sieve of Eratosthenes utilizing a modulo-30 factorization wheel ($2 \times 3 \times 5$), AVX2 256-bit SIMD periodic composite pre-blitting, and 4-way unrolled scalar elimination sweeps.

## Algorithmic Architecture

1. **Modulo-30 Wheel Factorization:**
   Only numbers coprime to 2, 3, and 5 are sieved. There are $\phi(30) = 8$ coprime residues per 30 integers: $\{1, 7, 11, 13, 17, 19, 23, 29\}$. Exactly 1 byte stores 8 candidate flags, compressing the $1,000,000$ sieve into only $33,334$ bytes ($\approx 32.55\text{ KB}$). This is 100% resident in L1D CPU cache (e.g. 48 KB on Intel Raptor Lake).

2. **AVX2 Periodic Pattern Blitting:**
   The composite flags of any coprime factor $p$ repeat with a cycle of exactly $p$ bytes ($8p$ bits):
   - **Prime 7:** Initialized directly during buffer clear via 256-bit `vmovdqa` across a 224-byte cycle (eliminating 142,857 composite markings).
   - **Primes 11, 13, 17, 19:** Composite multiples are blitted using 256-bit AVX2 vectors (`vmovdqu` + `vpor`) over periods of 352, 416, 544, and 608 bytes. This removes $>75\%$ of all memory writes from the scalar sieve loop.

3. **4-Way Decoupled Unrolled Marking:**
   For prime factors $p \ge 23$, candidate sweeps are unrolled 4-way with decoupled Load $\implies$ OR $\implies$ Store pipelines, completely avoiding read-modify-write stalls and store-forwarding hazards.

4. **Hardware `popcnt` Unrolling:**
   Prime counting scans 64-bit quadwords with 4 independent hardware `popcnt` accumulators to prevent Intel destination-register false dependency serialization.

## Implementations

- `tacitvs_st`: Single-threaded execution.
- `tacitvs_mt`: Multi-threaded execution across all available logical CPU cores.

## Build Instructions

```sh
chmod +x build.sh
./build.sh
```

Requirements: Linux x86-64 with AVX2 and BMI2 support (`-march=x86-64-v3`).

## Run Instructions

```sh
chmod +x run.sh
./run.sh
```

## Output Example

```log
TACITVS_st;137790;5.000021;1;algorithm=wheel,faithful=yes,bits=1
TACITVS_mt;614554;5.003112;24;algorithm=wheel,faithful=yes,bits=1
```
