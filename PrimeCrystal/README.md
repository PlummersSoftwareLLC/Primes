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
Iteration 01: Passes: 3955 Time: 5.000000 Avg: 0.001264 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 4369 Time: 5.000000 Avg: 0.001144 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 4334 Time: 5.000000 Avg: 0.001154 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 4344 Time: 5.000000 Avg: 0.001151 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 4352 Time: 5.000000 Avg: 0.001149 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 4365 Time: 5.000000 Avg: 0.001145 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 4357 Time: 5.000000 Avg: 0.001148 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 4286 Time: 5.000000 Avg: 0.001167 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 4370 Time: 5.000000 Avg: 0.001144 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 4372 Time: 5.000000 Avg: 0.001144 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 4356 Time: 5.000000 Avg: 0.001148 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 4339 Time: 5.000000 Avg: 0.001152 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 4328 Time: 5.000000 Avg: 0.001155 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 4339 Time: 5.000000 Avg: 0.001152 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 4343 Time: 5.000000 Avg: 0.001151 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 4363 Time: 5.000000 Avg: 0.001146 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 4375 Time: 5.000000 Avg: 0.001143 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 4373 Time: 5.000000 Avg: 0.001143 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 4369 Time: 5.000000 Avg: 0.001144 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 4358 Time: 5.000000 Avg: 0.001147 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 4375
```

Running it under Rosetta 2 is as easy as:

```
./run.sh
```

And here are the results:

```
Iteration 01: Passes: 3441 Time: 5.000000 Avg: 0.001453 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 4148 Time: 5.000000 Avg: 0.001205 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 4126 Time: 5.000000 Avg: 0.001212 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 4127 Time: 5.000000 Avg: 0.001212 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 4142 Time: 5.000000 Avg: 0.001207 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 4148 Time: 5.000000 Avg: 0.001205 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 4165 Time: 5.000000 Avg: 0.001200 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 4167 Time: 5.000000 Avg: 0.001200 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 4166 Time: 5.000000 Avg: 0.001200 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 4163 Time: 5.000000 Avg: 0.001201 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 4164 Time: 5.000000 Avg: 0.001201 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 4164 Time: 5.000000 Avg: 0.001201 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 4158 Time: 5.000000 Avg: 0.001203 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 4167 Time: 5.000000 Avg: 0.001200 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 4167 Time: 5.000000 Avg: 0.001200 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 4163 Time: 5.000000 Avg: 0.001201 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 4168 Time: 5.000000 Avg: 0.001200 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 4166 Time: 5.000000 Avg: 0.001200 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 4166 Time: 5.000000 Avg: 0.001200 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 4131 Time: 5.000000 Avg: 0.001210 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 4168
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
