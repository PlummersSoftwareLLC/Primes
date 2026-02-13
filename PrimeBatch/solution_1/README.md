# Batch solution by Sxxov & Ch2Laughlin

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A Windows Batch implementation of the prime sieve.  
Three worker implementations are included; the framework defaults to the fastest one.

---
## Algorithms Available

The framework can be switched to use any of the included workers.  
Given the debate around performance techniques in Batch, all three are provided so you can compare them on your own system.

### 🟢 Fastest — A[n] Loop Worker (Newton sqrt, no labels)
This is the fastest implementation (`worker.bat`).  

It avoids `goto` and labels in the hot path, relying instead on `for /L` loops, which CMD interprets far more efficiently.

Key characteristics:

- Uses short variable names (`A[n]`) to reduce environment‑variable overhead.
- Uses an optimized Newton integer square root (no loops required).
- Builds environment variables incrementally as composites are found similar to a sparse array being filled in.
- Produces correct prime lists.
- Bits used per candidate grows as the sieve size becomes larger so we don't claim a bit size for this algorithm.

### 🟡 Original Sxxov Implementation (goto/labels, broken sqrt)
The historical baseline (`worker_orig.bat`).  
This version attempted clever optimizations but ended up slower due to:

- Heavy use of `goto` and labels in the hot path.
- A broken square‑root implementation.
- Excessive environment‑variable churn.
- Sadly, it does not generate a correct prime list.

It is preserved for comparison and historical accuracy.

### 🔵 Packed Implementation (bitmap‑style)
The packed worker (`worker_packed.bat`) reduces environment‑variable usage by ~30×.  It has a constant bit size for each candidate using a set of environment variables defined at the start of the each worker_packed.bat run.

However:

- Bitmap logic requires more commands per composite.
- Initialization cost is high (all packed vars must be created up front).
- In practice, it is slower than the A[n] loop worker for all tested sieve sizes up to 1M.

It may become competitive above ~10M, but below that the interpreter overhead dominates.

## Framework

The framework imitates a class/worker model by spawning a new `cmd.exe` instance per pass calling the worker bat.  This allows multi‑worker execution (parallelism), though CMD’s overhead makes it extremely slow at large sieve sizes.

Because of this slowness, the framework is **not** included in automated benchmarking.  See the [Output](#output) and [Performance](#Performance) section for reference results.

---
## Run Instructions

Assuming you are in `PrimeBatch/solution_1` and have the dependencies from `BENCHMARK.md`:

Both arguments (`workers`, `sieveSize`) are optional.

Example (Windows):

```batch
main.bat /workers:%NUMBER_OF_PROCESSORS% /sieveSize:1000


## Run instructions

These instructions assumes that you're in the directory of `PrimeBatch/solution_1` & have all the dependencies installed as stated in `BENCHMARK.md`.

Both arguments `workers` & `sieveSize` can be omitted, they will default to the values shown in the examples below.

> Note: The examples & the defaults use the prime count size based on the rules. If you'd like to actually run these, it's probably better you decrease them. I found a sieve size of ~1000 gives the implementation a much better chance of going through more than 1 pass per 5 seconds. Example to run on Windows:
>
> ```
> main.bat /workers:%NUMBER_OF_PROCESSORS% /sieveSize:1000
> ```

### Natively on Windows

This uses `cmd.exe`, which is included by default in all versions of Windows.

```batch
main.bat /workers:1 /sieveSize:1000000
```

### Via Wine on Unix-based Systems

This uses `Wine`, which you may need to install. Refer [here](https://www.tecmint.com/install-wine-in-ubuntu/).

```bash
wine cmd /c main.bat /workers:1 /sieveSize:1000000
```

### Via Docker on Unix-based Systems

This uses `Docker`, which you should have already installed if coming from `BENCHMARK.md`.

```bash
docker build --build-arg workers=1 --build-arg sieveSize=1000000 -t primebatch . && docker run -d primebatch
```

---
## Performance

Below is a table of averaged times collected on my machine over several days using the default algorithm.

| Sieve Size | A[n] Worker  |
|-----------:|-------------:|
| 1k         | 0.12 s       |
| 5k         | 0.81 s       |
| 10k        | 1.69 s       |
| 50k        | 28.41 s      |
| 100k       | 106.26 s     |
| 500k       | 2,276.11 s   |
| 1M         | 9,937.02 s   |

## Output
This run was executed natively on Windows using the default worker implementation.
```
elapsed: 02:45:37.02 (9937.02s total)

---
batch;1;9937.02;1;algorithm=base,faithful=yes
```

Machine specifications:

* Intel i7-6820HQ @ 2.7GHz
* 32 GB 2133MHz LPDDR4
* Windows 10 22H2 (aka build 19045)
