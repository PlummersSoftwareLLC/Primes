# JavaScript solution 2 by Helron1977

This solution contains two different implementations of the Prime Sieve.

## Implementations

### 1. Sniper (God Mode)
- **Algorithm**: `other` (Static Mask for first 11 primes)
- **Faithfulness**: `no` (Uses a global buffer and pre-calculated mask)
- **Performance**: ~23,000 passes
- **Badges**:
  ![Algorithm](https://img.shields.io/badge/Algorithm-other-yellow)
  ![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)

### 2. Extreme (Super-Wheel)
- **Algorithm**: `other` (Wheel Factorization 3-13)
- **Faithfulness**: `yes` (Meets all faithful benchmark requirements)
- **Performance**: ~13,600 passes
- **Badges**:
  ![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
  ![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)

## Run instructions

```bash
./run.sh
```

This will run both implementations sequentially. You can also run them individually:

```bash
node PrimeJavaScript_sniper.js
node PrimeJavaScript_extreme_sieve.js
```

## Output format

```log
helron-sniper;[PASSES];[DURATION];1;algorithm=other,faithful=no,bits=1
helron-extreme;[PASSES];[DURATION];1;algorithm=other,faithful=yes,bits=1
```
