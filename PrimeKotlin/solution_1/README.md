# Prime Sieve Algorithms
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Kotlin Idiomatic Fast solution by wulkanat

This version is more focused on performance, but still relatively idiomatic.

Speed should be pretty close to what you get with a plain Java implementation.

One very unfortunate optimization that has to be done here is eliminating the `step` function.
Because Kotlin fails to optimize it (and creates a range object), we have to replace it with a `while` loop instead.

*Includes contributions by `Jakob K`*

## Results

Tested on a 6 Core i5 8400 and 16 GB 2400 MHz RAM

Results for Multithreaded Coroutines (**28566** iterations in 5 seconds):
```
Passes: 28566, Time: 5.0, Avg: 1.7503325631870056E-4, Limit: 1000000, Count: 78498, Valid: true

kotlin_idiomatic_fast_multithreaded;28566;5.0;1;algorithm=base,faithful=yes
```

Results for Singlethreaded (**5703** iterations in 5 seconds):
```
Passes: 5703, Time: 5.0, Avg: 8.76731544800982E-4, Limit: 1000000, Count: 78498, Valid: true

kotlin_idiomatic_fast_singlethreaded;5703;5.0;1;algorithm=base,faithful=yes
```