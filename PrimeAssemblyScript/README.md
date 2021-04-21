# AssemblyScript implementation


## Run instructions

### Running locally

Tested with node 14+

Install dependencies
```
npm install
```

Build the optimized binary
```
npm run build:bench
```

Run the benchmark
```
npm run bench
```

### Docker

You can also choose the easy option of running the application inside a Docker container.

```
docker build -t primes .
docker run --rm primes
```

## Known Issues

Currently the docker version almost halves the passes number vs the local version

## Benchmarks

### Ryzen 3900x / Node 15 / Windows 10

```
Passes: 8444, Time: 10.00100040435791, Avg: 0.0011843913234770299, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```
