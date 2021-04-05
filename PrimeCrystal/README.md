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
Iteration 01: Passes: 4015 Time: 5.000000 Avg: 0.001245 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 4268 Time: 5.000000 Avg: 0.001172 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 4265 Time: 5.000000 Avg: 0.001172 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 4230 Time: 5.000000 Avg: 0.001182 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 4169 Time: 5.000000 Avg: 0.001199 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 4254 Time: 5.000000 Avg: 0.001175 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 4221 Time: 5.000000 Avg: 0.001185 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 4182 Time: 5.000000 Avg: 0.001196 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 4211 Time: 5.000000 Avg: 0.001187 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 4214 Time: 5.000000 Avg: 0.001187 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 4249 Time: 5.000000 Avg: 0.001177 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 4253 Time: 5.000000 Avg: 0.001176 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 4191 Time: 5.000000 Avg: 0.001193 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 4192 Time: 5.000000 Avg: 0.001193 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 4239 Time: 5.000000 Avg: 0.001180 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 4207 Time: 5.000000 Avg: 0.001188 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 4229 Time: 5.000000 Avg: 0.001182 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 4227 Time: 5.000000 Avg: 0.001183 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 4195 Time: 5.000000 Avg: 0.001192 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 4250 Time: 5.000000 Avg: 0.001176 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 4268
```

Running it under Rosetta 2 is as easy as:

```
./run.sh
```

And here are the results:

```
Iteration 01: Passes: 3378 Time: 5.000000 Avg: 0.001480 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 3935 Time: 5.000000 Avg: 0.001271 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 3927 Time: 5.000000 Avg: 0.001273 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 3927 Time: 5.000000 Avg: 0.001273 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 3914 Time: 5.000000 Avg: 0.001277 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 3917 Time: 5.000000 Avg: 0.001276 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 3924 Time: 5.000000 Avg: 0.001274 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 3879 Time: 5.000000 Avg: 0.001289 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 3907 Time: 5.000000 Avg: 0.001280 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 3927 Time: 5.000000 Avg: 0.001273 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 3893 Time: 5.000000 Avg: 0.001284 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 3907 Time: 5.000000 Avg: 0.001280 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 3899 Time: 5.000000 Avg: 0.001282 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 3911 Time: 5.000000 Avg: 0.001278 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 3913 Time: 5.000000 Avg: 0.001278 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 3911 Time: 5.000000 Avg: 0.001278 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 3912 Time: 5.000000 Avg: 0.001278 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 3908 Time: 5.000000 Avg: 0.001279 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 3911 Time: 5.000000 Avg: 0.001278 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 3915 Time: 5.000000 Avg: 0.001277 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 3935
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
Iteration 01: Passes: 2434 Time: 5.000000 Avg: 0.002054 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 02: Passes: 2821 Time: 5.000000 Avg: 0.001772 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 03: Passes: 2792 Time: 5.000000 Avg: 0.001791 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 04: Passes: 2807 Time: 5.000000 Avg: 0.001781 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 05: Passes: 2818 Time: 5.000000 Avg: 0.001774 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 06: Passes: 2814 Time: 5.000000 Avg: 0.001777 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 07: Passes: 2809 Time: 5.000000 Avg: 0.001780 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 08: Passes: 2800 Time: 5.000000 Avg: 0.001786 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 09: Passes: 2815 Time: 5.000000 Avg: 0.001776 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 10: Passes: 2810 Time: 5.000000 Avg: 0.001779 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 11: Passes: 2812 Time: 5.000000 Avg: 0.001778 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 12: Passes: 2796 Time: 5.000000 Avg: 0.001788 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 13: Passes: 2799 Time: 5.000000 Avg: 0.001786 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 14: Passes: 2808 Time: 5.000000 Avg: 0.001781 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 15: Passes: 2804 Time: 5.000000 Avg: 0.001783 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 16: Passes: 2827 Time: 5.000000 Avg: 0.001769 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 17: Passes: 2801 Time: 5.000000 Avg: 0.001785 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 18: Passes: 2817 Time: 5.000000 Avg: 0.001775 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 19: Passes: 2787 Time: 5.000000 Avg: 0.001794 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Iteration 20: Passes: 2801 Time: 5.000000 Avg: 0.001785 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
-----
Best result is: 2827
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
