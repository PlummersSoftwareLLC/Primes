# Prime Sieve Algorithms
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Kotlin Idiomatic solution by wulkanat

This version is more focused on a readable, idiomatic way to write Kotlin code,
avoiding compiler workarounds and thus probably more representative of what you
would find in a real-world project.

*Includes contributions by `Jakob K`*

## Results

Tested on a 6 Core i5 8400 and 16 GB 2400 MHz RAM

Results for Multithreaded Coroutines (**26584** iterations in 5 seconds):
```
Passes: 26584, Time: 5.0, Avg: 1.8808305747818236E-4, Limit: 1000000, Count: 78498, Valid: true

kotlin_idiomatic_multithreaded;26584;5.0;1;algorithm=base,faithful=yes
```

Results for Singlethreaded (**5487** iterations in 5 seconds):
```
Passes: 5487, Time: 5.0, Avg: 9.11244760342628E-4, Limit: 1000000, Count: 78498, Valid: true

kotlin_idiomatic_singlethreaded;5487;5.0;1;algorithm=base,faithful=yes
```