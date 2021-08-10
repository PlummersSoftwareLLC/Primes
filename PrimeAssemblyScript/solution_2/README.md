# AssemblyScript implementation by MaxGraey

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Run instructions

### Running locally

Tested with node 16+

Install dependencies
```
npm install
```

Build the optimized binary
```
npm run build:orig
npm run build:unrolled
```

Run the benchmark
```
npm run bench:orig
npm run bench:unrolled
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

### Env

Node.JS: `v16.6.1`
CPU: `Intel Core i9 2.3 GHz`

### Results

Original:

```text
Passes: 7670,
Time: 5.0,
Avg: 0.0006518904701806605,
Limit: 1000000,
Count1: 78498,
Count2: 78498,
Valid: true
```

```bash
maxgraey;7670;5.0;1;algorithm=base,faithful=yes,bits=1
```

Unrolled:
```text
Passes: 12563,
Time: 5.0,
Avg: 0.00039799409569241107,
Limit: 1000000,
Count1: 78498,
Count2: 78498,
Valid: true
```

```bash
maxgraey_unrolled;12563;5.0;1;algorithm=base,faithful=yes,bits=1
```
