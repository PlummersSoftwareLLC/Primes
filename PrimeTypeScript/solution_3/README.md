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

Running on ArchLinux, nodejs:
npm run start

```
Passes: 5349, Time: 5, Avg: 0.0009347541596560105, Limit: 1000000, Count: 306344, Valid: false
trimvis_32bit-array;5349;5;1;algorithm=base,faithful=yes,bits=1
Passes: 5936, Time: 5, Avg: 0.0008423180592991914, Limit: 1000000, Count: 306344, Valid: false
trimvis_8bit-array;5936;5;1;algorithm=base,faithful=yes,bits=1
Passes: 645, Time: 5.008, Avg: 0.007764341085271318, Limit: 1000000, Count: 306344, Valid: false
trimvis_number-array;645;5.008;1;algorithm=base,faithful=yes
Passes: 8861, Time: 5, Avg: 0.0005642703983749013, Limit: 1000000, Count: 306344, Valid: false
trimvis_byte-array;8861;5;1;algorithm=base,faithful=yes,bits=8
```

## Author

- Original SOLUTION_1
Tudor Marghidanu
https://marghidanu.com/

- Original SOLUTION_2
Michael van der Bokke
http://vanderbokke.net/Michael/

- Original SOLUTION_3
TrimVis
