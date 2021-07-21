# Nim implementation #2

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Run

### Run locally

Nim is available via package manager under the popular systems. The following command should get you started:

```
nim c -d:danger --passC:"-march=native" -d:lto --gc:arc primes.nim
```

### Docker

As per usual minimal just a minimal set of commands:

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks
Gains a substantial performance boost from changing the GC from the default refc to the arc GC.
Gains a minor performance boost from removing the usage of `method`, changing the base type to `uint64`.

## Output
```
?
```

