# AssemblyScript implementation by donmahallem

![Category](https://img.shields.io/badge/Category-faithful-green)

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

| System | Passes | Time | Avg | Limit | Count1 | Count2 | Valid |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Ryzen 3900x / Node 14.16.1 / Windows 10 | 8796 | 10.0010 | 0.0012 | 1000000 | 78498 | 78498 | true |
