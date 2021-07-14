# Go solution by jdemchuk

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

1-bit single threaded Go solution.
Not as fast as Go solution_2 but the guy who wrote that is amazing! I'm still trying to figure that one out.
I followed Dave Plummer's C++ example very closely.

Written by another guy from Saskatchewan.

## Run instructions

```
go run main.go [args]
```
Command line args:

`-limit X`: Size of the prime sieve. default = 1,000,000

`-time Xs`: Time to run in seconds. default = 5s

`-verbose`: print results include a list of all prime numbers

## Output
Intel Core i7-6600U @ 2.6GHz
```
Passes: 2309, Time: 5.0016139s, Avg: 0.002166s, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

jdemchukprime-go;2309;5.0016139;1;algorithm=base,faithful=yes,bits=1
```