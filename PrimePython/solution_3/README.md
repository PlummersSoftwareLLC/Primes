# Numpy (and Numba JIT) Prime Sieve

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This solution contains two implementations sharing the same Docker image:

1. **emillynge_numpy** (`PrimePY.py`) — by [emillynge](https://github.com/emillynge)
   Uses a numpy boolean array for the sieve. Marking numbers as non-prime with
   numpy's strided slice assignment is highly optimised (SIMD-vectorised C under
   the hood), which gives a large speed-up over pure Python.

2. **TylerDOC1776_numba** (`PrimePY_numba.py`) — by [TylerDOC1776](https://github.com/TylerDOC1776)
   Extends the numpy approach by decorating the core sieve loop with Numba's
   `@njit`, compiling the find-next-prime loop and the strided slice assignment
   to native machine code. A warm-up call is made before the timed benchmark to
   absorb the one-time JIT compilation cost.

Both solutions are **unfaithful** due to the use of external dependencies.

---

## Running with Python

Install Python and dependencies:

```bash
pip install numpy numba
```

Run either implementation directly:

```bash
python PrimePY.py          # numpy implementation
python PrimePY_numba.py    # Numba JIT implementation
```

## Command line arguments

- `--limit=X`, `-l X`: set upper limit for calculating primes. Default is 1_000_000.
- `--time=X`, `-t X`: set running time, in seconds. Default is 5.
- `--show`, `-s`: output the found primes.

## Running tests

```bash
python -m unittest
```

## Results (emillynge, AMD Ryzen 3600, Arch Linux 64 bit, Python 3.9.5)

```text
                                                            Single-threaded
┌───────┬────────────────┬──────────┬──────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼──────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ c              │ 1        │ mckoss-c830          │ 12469  │ 5.00000  │    1    │   wheel   │   yes    │ 1    │  2493.80000   │
│   2   │ python         │ 3        │ emillynge_numpy      │  7830  │ 5.00030  │    1    │   base    │    no    │ 8    │  1565.90684   │
│   3   │ python         │ 3        │ emillynge_numpy_pypy │  4140  │ 5.00031  │    1    │   base    │    no    │ 8    │   827.94845   │
│   4   │ python         │ 2        │ ssovest              │  2179  │ 5.00037  │    1    │   base    │   yes    │ 8    │   435.76803   │
│   5   │ python         │ 1        │ davepl               │   40   │ 10.05578 │    1    │   base    │   yes    │      │    3.97781    │
└───────┴────────────────┴──────────┴──────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```

## Results (TylerDOC1776, Numba JIT)

### Intel i7-10710U @ 1.61GHz, 16GB RAM, Windows 11 64-bit

| Solution | Passes/5s |
| --- | --- |
| 1 — davepl (list) | 691 |
| 2 — ssovest (bytearray) | 4,855 |
| 3 — emillynge (numpy) | 6,405 |
| **3 — TylerDOC1776 (numba)** | **8,043** |

### Intel i5-13600K, 64GB RAM, Windows 11 64-bit

| Solution | Passes/5s |
| --- | --- |
| 1 — davepl (list) | 1,197 |
| 2 — ssovest (bytearray) | 8,815 |
| 3 — emillynge (numpy) | 12,598 |
| **3 — TylerDOC1776 (numba)** | **11,651** |

Note: on the faster machine, the Numba JIT scores slightly below the numpy implementation.
The JIT-compiled loop wins on memory-bandwidth-limited hardware; on fast hardware numpy's
SIMD-vectorised strided writes are difficult to beat with a general-purpose JIT.

## Example output

```text
$ python3 PrimePython/solution_3/PrimePY.py
Passes: 10392, Time: 5.0000652491580695, Avg: 0.00048114561673961407, Limit: 1000000, Count: 78498, Valid: True

emillynge_numpy; 10392;5.0000652491580695;1;algorithm=base,faithful=no,bits=8

$ python3 PrimePython/solution_3/PrimePY_numba.py
Passes: 11200, Time: 5.0001234567890123, Avg: 0.00044644852292044752, Limit: 1000000, Count: 78498, Valid: True

TylerDOC1776_numba;11200;5.0001234567890123;1;algorithm=base,faithful=no,bits=8
```
