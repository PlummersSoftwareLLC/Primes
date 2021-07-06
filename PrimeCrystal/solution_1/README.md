# Crystal implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Run instructions

### Running locally

Crystal is available for several operating systems. Check out the Crystal install guide here: https://crystal-lang.org/install/. You can build and execute the application using the following commands:

```
crystal build primes.cr --release --no-debug
./primes
```

The `--release` flag will turn on all optimizations; this ensures we run as fast as possible.

### Docker

You can also choose the easy option of running the application inside a Docker container.

```
docker build -t primes .
docker run -ti --rm primes
```

## Known limitations

I'm currently using the built-in `BitArray` implementation for storing boolean values. The size of the bit array is limited to `Int32.MAX` (2147483647). If we plan on using large numbers in the future, I will have to re-implement this class.

## Benchmarks

### Running under M1

**Machine:** MacBook Air (M1, 2020)<br/>
**Processor:** Chip Apple M1<br/>
**Memory** 8GB<br/>

There's no **Crystal** build for the M1 yet, but I managed to get a native executable by using Crystal's cross-compilation feature. Running the benchmark with five iterations yields the following results:

```
Passes: 4233 Time: 5.000000 Avg: 0.001181 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4701 Time: 5.000000 Avg: 0.001064 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4704 Time: 5.000000 Avg: 0.001063 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4664 Time: 5.000000 Avg: 0.001072 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4698 Time: 5.000000 Avg: 0.001064 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
```

Running it under Rosetta, we get back some interesting data:

```
Passes: 4676 Time: 5.000000 Avg: 0.001069 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4948 Time: 5.000000 Avg: 0.001011 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4947 Time: 5.000000 Avg: 0.001011 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4948 Time: 5.000000 Avg: 0.001011 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4946 Time: 5.000000 Avg: 0.001011 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
```

GASP! It looks like the numbers are better under Rosetta!

### Running under Intel

**Machine:** MacBook Pro (13-inch, 2018, Four Thunderbolt 3 Ports)<br/>
**Processor:** 2.3 GHz Quad-Core Intel Core i5<br/>
**Memory:** 16GB 2133 MHz LPDDR3<br/>

```
Passes: 4580 Time: 5.000000 Avg: 0.001092 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4992 Time: 5.000000 Avg: 0.001002 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4985 Time: 5.000000 Avg: 0.001003 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4822 Time: 5.000000 Avg: 0.001037 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 4982 Time: 5.000000 Avg: 0.001004 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
```

### Raspberry PI 

**Machine:** Raspberry PI 4B 8GB<br/>
**Processor:** 1.5 GHz Cortex A72 64bit<br/>
**Memory:** 8GB LPDDR4 SDRAM<br/>

The Raspberry PI is running Ubuntu 20.10 and is using standard factory settings.

```
Passes: 1799 Time: 5.000000 Avg: 0.002779 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 1852 Time: 5.000000 Avg: 0.002700 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 1838 Time: 5.000000 Avg: 0.002720 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 1839 Time: 5.000000 Avg: 0.002719 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
Passes: 1843 Time: 5.000000 Avg: 0.002713 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
```

## Author

Tudor Marghidanu
https://marghidanu.com/