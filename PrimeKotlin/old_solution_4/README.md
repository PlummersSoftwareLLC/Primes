# Prime Sieve Algorithms
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Kotlin Fast solution by Jakob K
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)

A simple implementation of the base algorithm, that doesn't make use
of any special Kotlin features (apart from coroutines)

*Includes contributions by `wulkanat`*

## Results

Tested on a 6 Core i5 8400 and 16 GB 2400 MHz RAM

Results for Multithreaded Coroutines (**27302** iterations in 5 seconds):
```
Passes: 27302, Time: 5.0, Avg: 1.8313676653725002E-4, Limit: 1000000, Count: 78498, Valid: true

kotlin_traditional_multithreaded;27302;5.0;1;algorithm=base,faithful=yes
```

Results for Singlethreaded (**5921** iterations in 5 seconds):
```
Passes: 5921, Time: 5.0, Avg: 8.444519506840061E-4, Limit: 1000000, Count: 78498, Valid: true

kotlin_traditional_singlethreaded;5921;5.0;1;algorithm=base,faithful=yes
```