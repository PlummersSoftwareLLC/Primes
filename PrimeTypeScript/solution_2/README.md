# TypeScript implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Run

### Run locally

You will need Node.js installed on your local machine. To build and run locally, just run the following commands:

```
npm install
npm run dev
```

### Docker

If you don't want to install Node.js locally, just build it and run it inside Docker:

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks

Running on Windows 10, nodejs:
npm run start

```
Passes: 4240, Time: 5, Avg: 0.0011792452830188679, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
mikevdbokke/32bit-array;4240;5;1;algorithm=base,faithful=yes
Passes: 4265, Time: 5, Avg: 0.0011723329425556857, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
mikevdbokke/32bit-array;4265;5;1;algorithm=base,faithful=yes
Passes: 4277, Time: 5, Avg: 0.0011690437222352116, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
mikevdbokke/32bit-array;4277;5;1;algorithm=base,faithful=yes
Passes: 4230, Time: 5, Avg: 0.001182033096926714, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
mikevdbokke/32bit-array;4230;5;1;algorithm=base,faithful=yes
```

Running on Windows 10, nodejs: (all)
npm run start-all
```
Passes: 191, Time: 5.025, Avg: 0.026308900523560212, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
mikevdbokke/number-array;191;5.025;32;algorithm=base,faithful=yes
Passes: 3218, Time: 5.001, Avg: 0.0015540708514605345, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
mikevdbokke/Solution_1;3218;5.001;8;algorithm=base,faithful=yes
Passes: 3208, Time: 5, Avg: 0.0015586034912718205, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
mikevdbokke/byte-array;3208;5;8;algorithm=base,faithful=yes
Passes: 3214, Time: 5, Avg: 0.0015556938394523958, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
mikevdbokke/Solution_1;3214;5;8;algorithm=base,faithful=yes
Passes: 4029, Time: 5.001, Avg: 0.0012412509307520476, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
mikevdbokke/8bit-array;4029;5.001;1;algorithm=base,faithful=yes
Passes: 3962, Time: 5.001, Avg: 0.0012622412922766281, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
mikevdbokke/32bit-array;3962;5.001;1;algorithm=base,faithful=yes
```

## Author

- Original SOLUTION_1
Tudor Marghidanu
https://marghidanu.com/

- Original SOLUTION_2
Michael van der Bokke
http://vanderbokke.net/Michael/