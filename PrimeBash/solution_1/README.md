# Bash solution based on sources from Nitepone

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Modified by rbergen as indicated in the source code. Optimizations by Braden "Blzut3" Obrzut.

Contains multiple implemenations as follows:

1. An optimized base implementation written with natural code.
2. A variation of the base implementation where the bit accessing functions are inlined.

All implementations take advantage of bash arithmetic interpreting an empty string (i.e. uninitialized array index) as 0. The performance of bash in math heavy scripts seems to largely be determined by minimizing the number of variable accesses/writes and function calls. Of course avoiding the expensive operation of processing forking by eliminating subshells is also important.
