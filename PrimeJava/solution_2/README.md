# Java solution by PratimGhosh86

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

*This implementation follows the original implementation 1:1 by Dave and uses BitSet rather than arrays. Single threaded performance is slower compared to arrays but we are no longer limited to `Integer.MAX_VALUE - 2` (Requested array size exceeds VM limit).*

## Run instructions

*Java 11+ should be available in the system path if you want to run manually*
*For manual execution, `run-sieve` or `run-sieve.cmd` based on your enviroment*
*Performance varies based on JVM. `GraalVM` provides the best performance, followed by `hotspot`. Although `openj9` is the slowest, it is the most memory efficient*

## Output

```
> Executing task: docker run --rm -it  solution2:latest <

Passes: 2504, Time: 5.000000, Avg: 0.001997, Limit: 1000000, Count: 78498, Valid: true

PratimGhosh86;2504;5.000000;1;algorithm=base,faithful=yes,bits=1
```
