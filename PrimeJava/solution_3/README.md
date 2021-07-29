# Java solution by AndrewT based on code by MansenC

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

*This implementation follows the original implementation 1:1 by MansenC which is 1:1 with the implementation by Dave and uses bit twiddling rather than an array of bytes or a BitSet. Single threaded performance is faster than the other two implementations.

## Run instructions

*Java 11+ should be available in the system path if you want to run manually*
*Performance varies based on JVM. `GraalVM` provides the best performance, followed by `hotspot`. `openj9` is the most memory efficient, and the slowest*

javac PrimeSieveJava.java
java PrimeSieveJava

Should compile and ruin it just fine.

## Output

```
Executing task: docker run --rm -it  solution2:latest

Passes: 4893, Time: 5.000000, Avg: 0.001022, Limit: 1000000, Count: 78498, Valid: true

AndrewT;4893;5.000000;1;algorithm=base,faithful=yes
```
