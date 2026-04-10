# Lateralus solution by @bad-antics

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A faithful implementation of the prime sieve in [Lateralus](https://github.com/bad-antics/lateralus-lang), a modern expressive programming language featuring type inference, pipeline operators, and pattern matching.

## Lateralus Language Features Used

- **Type inference** with optional explicit types
- **Pipeline operators** (`|>`) for functional composition
- **Pattern matching** with Option types
- **Class-based** structure with methods
- **BitArray** from standard library for efficient 1-bit storage

## Run Instructions

### Using Lateralus compiler directly

```bash
# Install Lateralus (if not installed)
curl -fsSL https://lateralus.dev/install.sh | sh

# Run the solution
lateralus run primes.ltl
```

### Using Docker

```bash
docker build -t primes-lateralus .
docker run --rm primes-lateralus
```

## Output

```
Passes: 4521, Time: 5.000124s, Avg: 0.001105982s, Limit: 1000000, Count: 78498, Valid: valid

bad-antics;4521;5.000124;1;algorithm=base,faithful=yes,bits=1
```

## Notes

Lateralus compiles to native code via LLVM, achieving competitive performance with C and Rust while maintaining high-level expressiveness. The BitArray implementation uses 1-bit storage per prime candidate.
