# Crystal implementation

## Description

I have changed the game rules a little bit; the `run.sh` script will actually run 20 iterations of the program sequentially.

## Crystal Installation

Crystal is available for several operating systems. Please follow the installation guide for your or from the following link: https://crystal-lang.org/install/

## Benchmarks

## Running under M1

### Running it natively

I managed to build a native executable for M1 using Crystal cross-compilation.

```
./compile_m1.sh
NO_COMPILE=1 ./run.sh
```

```
Iteration 01: Passes: 3628 Time: 5.000000 Avg: 0.001378 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 4426 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 4419 Time: 5.000000 Avg: 0.001131 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 4425 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 4424 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 4405 Time: 5.000000 Avg: 0.001135 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 4423 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 4426 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 4426 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 4441 Time: 5.000000 Avg: 0.001126 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 4424 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 4414 Time: 5.000000 Avg: 0.001133 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 4420 Time: 5.000000 Avg: 0.001131 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 4410 Time: 5.000000 Avg: 0.001134 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 4428 Time: 5.000000 Avg: 0.001129 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 4423 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 4425 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 4424 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 4420 Time: 5.000000 Avg: 0.001131 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 4419 Time: 5.000000 Avg: 0.001131 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 4441
```

NOTE: The processor seems to be limited as the execution is not spiking on any of the cores.

### Running in Docker

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
Iteration 01: Passes: 3768 Time: 5.000000 Avg: 0.001327 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 4391 Time: 5.000000 Avg: 0.001139 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 4384 Time: 5.000000 Avg: 0.001141 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 4379 Time: 5.000000 Avg: 0.001142 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 4439 Time: 5.000000 Avg: 0.001126 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 4383 Time: 5.000000 Avg: 0.001141 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 4441 Time: 5.000000 Avg: 0.001126 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 4374 Time: 5.000000 Avg: 0.001143 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 4425 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 4426 Time: 5.000000 Avg: 0.001130 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 4439 Time: 5.000000 Avg: 0.001126 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 4383 Time: 5.000000 Avg: 0.001141 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 4376 Time: 5.000000 Avg: 0.001143 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 4385 Time: 5.000000 Avg: 0.001140 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 4389 Time: 5.000000 Avg: 0.001139 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 4385 Time: 5.000000 Avg: 0.001140 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 4386 Time: 5.000000 Avg: 0.001140 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 4383 Time: 5.000000 Avg: 0.001141 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 4392 Time: 5.000000 Avg: 0.001138 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 4384 Time: 5.000000 Avg: 0.001141 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 4441
```

### Running under Rosetta 2

Running it under Rosetta 2 is as easy as:

```
./run.sh
```

And here are the results:

```
Iteration 01: Passes: 3785 Time: 5.000000 Avg: 0.001321 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 4051 Time: 5.000000 Avg: 0.001234 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 4076 Time: 5.000000 Avg: 0.001227 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 4078 Time: 5.000000 Avg: 0.001226 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 4051 Time: 5.000000 Avg: 0.001234 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 4080 Time: 5.000000 Avg: 0.001225 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 4105 Time: 5.000000 Avg: 0.001218 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 4083 Time: 5.000000 Avg: 0.001225 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 4038 Time: 5.000000 Avg: 0.001238 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 4071 Time: 5.000000 Avg: 0.001228 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 4048 Time: 5.000000 Avg: 0.001235 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 4066 Time: 5.000000 Avg: 0.001230 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 4046 Time: 5.000000 Avg: 0.001236 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 4083 Time: 5.000000 Avg: 0.001225 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 4090 Time: 5.000000 Avg: 0.001222 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 4093 Time: 5.000000 Avg: 0.001222 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 4088 Time: 5.000000 Avg: 0.001223 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 4098 Time: 5.000000 Avg: 0.001220 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 4095 Time: 5.000000 Avg: 0.001221 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 4090 Time: 5.000000 Avg: 0.001222 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 4105
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
Iteration 01: Passes: 2173 Time: 5.000000 Avg: 0.002301 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 2325 Time: 5.000000 Avg: 0.002151 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 2322 Time: 5.000000 Avg: 0.002153 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 2212 Time: 5.000000 Avg: 0.002260 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 2242 Time: 5.000000 Avg: 0.002230 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 2322 Time: 5.000000 Avg: 0.002153 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 2321 Time: 5.000000 Avg: 0.002154 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 2304 Time: 5.000000 Avg: 0.002170 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 2312 Time: 5.000000 Avg: 0.002163 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 2324 Time: 5.000000 Avg: 0.002151 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 2315 Time: 5.000000 Avg: 0.002160 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 2310 Time: 5.000000 Avg: 0.002165 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 2322 Time: 5.000000 Avg: 0.002153 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 2305 Time: 5.000000 Avg: 0.002169 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 2321 Time: 5.000000 Avg: 0.002154 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 2310 Time: 5.000000 Avg: 0.002165 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 2317 Time: 5.000000 Avg: 0.002158 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 2320 Time: 5.000000 Avg: 0.002155 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 2324 Time: 5.000000 Avg: 0.002151 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 2307 Time: 5.000000 Avg: 0.002167 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 2325
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
