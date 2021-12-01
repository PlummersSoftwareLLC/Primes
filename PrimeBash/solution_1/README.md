# Bash solution based on sources from Nitepone

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Modified by rbergen as indicated in the source code. Optimizations by Braden "Blzut3" Obrzut.

Contains multiple implementations as follows:

1. An optimized base implementation written with natural code.
2. A variation of the base implementation where the bit accessing functions are inlined.
3. Version using 64-bit bit packing. Although 64-bit arithmetic is used, Bash still stores base-10 strings.

All implementations take advantage of bash arithmetic interpreting an empty string (i.e. uninitialized array index) as 0. The performance of bash in math heavy scripts seems to largely be determined by minimizing the number of variable accesses/writes and function calls. Of course avoiding the expensive operation of processing forking by eliminating subshells is also important.

The bit-packed version could also take advantage of the inlining optimization, but that variant is not included here since the cost of doing the manipulations required to bit-pack easily outweigh the space savings. This implementation is included just to show it can be done.
