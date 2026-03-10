# JavaScript solution 3 by Helron1977

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Description

Optimized sieve implementation using:
- **Wheel Factorization** (2, 3, 5, 7, 11, 13) via Super-Wheel strategy.
- **Word-Aligned** memory initialization for ultra-fast startup.
- **Localized Scope** (Hoisting) for V8 register optimization.
- **Loop Unrolling x32** to maximize the execution pipeline.

The main script is **PrimeJavaScript_extreme_sieve.js**, which implements a "Super-Wheel" covering primes 3-13 and an unrolled sieving loop.

To run:
```bash
./run.sh
```
or
```bash
node PrimeJavaScript_extreme_sieve.js
```

## Output

```log
helron-final;[PASSES];[DURATION];1;algorithm=other,faithful=yes,bits=1
```
