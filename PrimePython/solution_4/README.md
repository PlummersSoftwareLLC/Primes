# Numba JIT Prime Sieve

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Based on the original Python sieve by Dave Plummer, adapted for numpy by emillynge (solution_3).
Further adapted by Tyler/DOC (TylerDOC1776) — the sieve logic is identical to solution_3 but the
core function is decorated with Numba's `@njit`, compiling the find-next-prime
loop and the strided slice assignment to native machine code.

The solution is **unfaithful** due to the use of external dependencies (numpy + numba).

JIT compilation happens on the first call. The benchmark absorbs this cost via a
warm-up call before the timer starts.

## Run instructions

```bash
pip install numba numpy
python PrimePY.py
```

## Output

```text
numba_sieve;<passes>;5.001;1;algorithm=base,faithful=no,bits=8
```

## Results

### Intel i7-10710U @ 1.61GHz, 16GB RAM, Windows 11 64-bit

| Solution | Passes/5s |
| --- | --- |
| 1 — davepl (list) | 691 |
| 2 — ssovest (bytearray) | 4,855 |
| 3 — emillynge (numpy) | 6,405 |
| **4 — numba JIT** | **8,043** |

### Intel i5-13600K, 64GB RAM, Windows 11 64-bit

| Solution | Passes/5s |
| --- | --- |
| 1 — davepl (list) | 1,197 |
| 2 — ssovest (bytearray) | 8,815 |
| 3 — emillynge (numpy) | 12,598 |
| **4 — numba JIT** | **11,651** |

Note: on the faster machine, solution_4 scores slightly below solution_3. The JIT-compiled loop
wins on memory-bandwidth-limited hardware; on fast hardware numpy's SIMD-vectorized strided
writes are difficult to beat with a general-purpose JIT.
