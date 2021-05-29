# Nim implementation #2

![Category](https://img.shields.io/badge/Category-faithful-green)

## Run

### Run locally

Nim is available via package manager under the popular systems. The following command should get you started:

```
nim c -d:danger --passC:"-march=native" -d:lto -r primes.nim
```

### Docker

As per usual minimal just a minimal set of commands:

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks
Gains around 10% performance from `solution_1` with just `-d:release`. With added flags around 50% faster.

## Output
```
beef331;5398;5.00043797492981;1
```

