# Nim implementation #2

![Category](https://img.shields.io/badge/Category-faithful-green)

## Run

### Run locally

Nim is available via package manager under the popular systems. The following command should get you started:

```
nim c -d:danger -r primes.nim
```

### Docker

As per usual minimal just a minimal set of commands:

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks
Gains around 10% performance from `solution_1` with just `-d:release`, doing `-d:danger` it becomes ~135% faster here.

## Output
```
beef331;5398;5.00043797492981;1
```

