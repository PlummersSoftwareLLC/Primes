# Crystal implementation

## Description

I have changed the game rules a little bit; the `run.sh` script will actually run 20 iterations of the program sequentially.

## Crystal Installation

Crystal is available for several operating systems. Please follow the installation guide for your or from the following link: https://crystal-lang.org/install/

## Benchmarks

## Running under M1

```
MacBook Air (M1, 2020)
Chip Apple M1
Memory 8GB
```

Unfortunately, the Crystal compiler is not ready for the M1 processor. But I've tested it on an M1 Macbook Air inside Docker. The application was compiled under the Alpine Docker image generating an **ARM aarch64** executable:

```
docker-compose build
docker-compose run --rm primes
```

The results are:

```
Iteration 01: Passes: 3656 Time: 5.000000 Avg: 0.001368 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 4427 Time: 5.000000 Avg: 0.001129 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 4420 Time: 5.000000 Avg: 0.001131 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 4430 Time: 5.000000 Avg: 0.001129 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 4432 Time: 5.000000 Avg: 0.001128 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 4422 Time: 5.000000 Avg: 0.001131 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 4431 Time: 5.000000 Avg: 0.001128 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 4422 Time: 5.000000 Avg: 0.001131 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 4424 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 4431 Time: 5.000000 Avg: 0.001128 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 4425 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 4426 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 4431 Time: 5.000000 Avg: 0.001128 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 4423 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 4420 Time: 5.000000 Avg: 0.001131 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 4425 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 4425 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 4432 Time: 5.000000 Avg: 0.001128 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 4436 Time: 5.000000 Avg: 0.001127 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 4430 Time: 5.000000 Avg: 0.001129 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 4436
```

Running it under Rosetta 2 is as easy as:

```
./run.sh
```

And here are the results:

```
Iteration 01: Passes: 3735 Time: 5.000000 Avg: 0.001339 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 4192 Time: 5.000000 Avg: 0.001193 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 4199 Time: 5.000000 Avg: 0.001191 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 4194 Time: 5.000000 Avg: 0.001192 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 4197 Time: 5.000000 Avg: 0.001191 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 4197 Time: 5.000000 Avg: 0.001191 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 4190 Time: 5.000000 Avg: 0.001193 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 4190 Time: 5.000000 Avg: 0.001193 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 4198 Time: 5.000000 Avg: 0.001191 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 4197 Time: 5.000000 Avg: 0.001191 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 4196 Time: 5.000000 Avg: 0.001192 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 4197 Time: 5.000000 Avg: 0.001191 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 4192 Time: 5.000000 Avg: 0.001193 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 4197 Time: 5.000000 Avg: 0.001191 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 4197 Time: 5.000000 Avg: 0.001191 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 4196 Time: 5.000000 Avg: 0.001192 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 4196 Time: 5.000000 Avg: 0.001192 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 4182 Time: 5.000000 Avg: 0.001196 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 4166 Time: 5.000000 Avg: 0.001200 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 4170 Time: 5.000000 Avg: 0.001199 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 4199
```

## Running under Intel

```
Machine: MacBook Pro (13-inch, 2018, Four Thunderbolt 3 Ports)
Processor: 2.3 GHz Quad-Core Intel Core i5
Memory: 16GB 2133 MHz LPDDR3
```

To run the benchmark just use the `run.sh` utility script

```
./run.sh
```

And here are the results:

```
Iteration 01: Passes: 2856 Time: 5.000000 Avg: 0.001751 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 2984 Time: 5.000000 Avg: 0.001676 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 3029 Time: 5.000000 Avg: 0.001651 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 3043 Time: 5.000000 Avg: 0.001643 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 3049 Time: 5.000000 Avg: 0.001640 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 3054 Time: 5.000000 Avg: 0.001637 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 3050 Time: 5.000000 Avg: 0.001639 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 3054 Time: 5.000000 Avg: 0.001637 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 3062 Time: 5.000000 Avg: 0.001633 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 3077 Time: 5.000000 Avg: 0.001625 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 3061 Time: 5.000000 Avg: 0.001633 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 3037 Time: 5.000000 Avg: 0.001646 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 3059 Time: 5.000000 Avg: 0.001635 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 3060 Time: 5.000000 Avg: 0.001634 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 3042 Time: 5.000000 Avg: 0.001644 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 3071 Time: 5.000000 Avg: 0.001628 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 3045 Time: 5.000000 Avg: 0.001642 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 3072 Time: 5.000000 Avg: 0.001628 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 3059 Time: 5.000000 Avg: 0.001635 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 3069 Time: 5.000000 Avg: 0.001629 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 3077
```

## Running it under Raspberry Pi

```
Raspberry PI 4B 8GB
Processor: 1.5 GHz Cortex A72 64bit
Memory: 8 GB LPDDR4 SDRAM
```

Still waiting on the kit to arrive; I will add the numbers after the machine is setup.

# Author

Tudor Marghidanu
