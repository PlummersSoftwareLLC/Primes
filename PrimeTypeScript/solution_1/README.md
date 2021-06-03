# TypeScript implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
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

Here are some numbers from running inside Docker on my M1 machine:

```
Passes: 927, Time: 5, Avg: 0.005393743257820928, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 942, Time: 5, Avg: 0.005307855626326964, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 955, Time: 5, Avg: 0.005235602094240838, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 953, Time: 5, Avg: 0.005246589716684155, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 948, Time: 5, Avg: 0.005274261603375527, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 943, Time: 5, Avg: 0.005302226935312832, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 941, Time: 5, Avg: 0.005313496280552604, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 945, Time: 5, Avg: 0.005291005291005291, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 931, Time: 5, Avg: 0.0053705692803437165, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 934, Time: 5, Avg: 0.0053533190578158455, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```

## Author

Tudor Marghidanu
https://marghidanu.com/