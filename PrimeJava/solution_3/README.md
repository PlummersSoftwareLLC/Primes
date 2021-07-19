# Java BitSet (with Executors) solution by PratimGhosh86

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

*This implementation keeps the sieve calculation mechanish as close to the original implementation, using BitSets rather than arrays. Instead of patitioning the sieve size, each thread is responsible of handling the entire range and emitting the resultant sieve size at completion. To validate, we use the average of the prime count from every thread against a predefined dictionary*

## Run instructions

*Java 11+ should be available in the system path if you want to run manually. Performance varies based on JVM; `GraalVM` provides the best performance, followed by `hotspot`. `openj9` is the most memory efficient, and the slowest*

## Output

```
> Executing task: docker run --rm -it  solution3:latest <

Passes: 10150, Time: 5.000000, Avg: 0.000493, Limit: 1000000, Count: 78498, Valid: true

PratimGhosh86-solution_3;10150;5.000000;212;algorithm=base,faithful=yes,bits=1
```
