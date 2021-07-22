# Java BitSet (with Executors) solution by PratimGhosh86

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

*The implementations provided using BitSet are as follows:*

*`PrimeSieveJavaBitSet.java`- This implementation follows the original implementation 1:1 by Dave and uses BitSet rather than arrays. Single threaded performance is slower compared to arrays but we are no longer limited to Integer.MAX_VALUE - 2 (Requested array size exceeds VM limit).*

*`PrimeSieveJavaBitSetMT.java` - This implementation keeps the sieve calculation mechanism close to the original implementation, using BitSets rather than arrays. Instead of partitioning the sieve by a given size, each thread is responsible of handling the entire range and emitting the resultant sieve size at completion. To validate, we use the average of the prime count from every thread against a predefined dictionary*

## Run instructions

*Java 11+ should be available in the system path if you want to run manually. Performance varies based on JVM; `GraalVM` provides the best performance, followed by `hotspot`. `openj9` is the most memory efficient, and the slowest*

## Output

```
> Executing task: docker run --rm -it  solution2:latest <

PratimGhosh86-JavaBitSet;1915;5.000000;1;algorithm=base,faithful=yes,bits=1
PratimGhosh86-JavaBitSetMT;12496;5.000000;236;algorithm=base,faithful=yes,bits=1
```
