# Java solution by MansenC compiled to native code by jagodevreede

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This solution is a copy paste from solution 1. Except this solution will run native code, instead of the interpreted java code.

This will demonstrate the current state of native compilation. As the code is not optimized as is the case with hot-spot, this solution will run slower than the original. 

For more information about native images see (https://www.graalvm.org/reference-manual/native-image/)

## Output

```
> Executing task: docker run --rm -it  solution3:latest <

Passes: 3356, Time: 5.001000, Avg: 0.001490, Limit: 1000000, Count: 78498, Valid: true

MansenC-native;3356;5.001000;1;algorithm=base,faithful=yes
```
