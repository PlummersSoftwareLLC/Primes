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
Passes: 4177, Time: 5, Avg: 0.0011970313622216902, Limit: 1000000, Count: 78498, Valid: true
mikevdbokke_32bit-array;4177;5;1;algorithm=base,faithful=yes,bits=1
Passes: 3913, Time: 5, Avg: 0.001277791975466394, Limit: 1000000, Count: 78498, Valid: true
mikevdbokke_8bit-array;3913;5;1;algorithm=base,faithful=yes,bits=1
Passes: 208, Time: 5.007, Avg: 0.024072115384615383, Limit: 1000000, Count: 78498, Valid: true
mikevdbokke_number-array;208;5.007;1;algorithm=base,faithful=yes,bits=unknown
Passes: 3225, Time: 5, Avg: 0.0015503875968992248, Limit: 1000000, Count: 78498, Valid: true
mikevdbokke_byte-array;3225;5;1;algorithm=base,faithful=yes,bits=8
```

## Author

- Original SOLUTION_1
Tudor Marghidanu
https://marghidanu.com/

- Original SOLUTION_2
Michael van der Bokke
http://vanderbokke.net/Michael/