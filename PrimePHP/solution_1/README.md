# PHP solution by DennisdeBest

Optimizations by xeridea:
No longer uses class methods for basic operations.
Leaned the loop logic.
Uses factor * factor.
Enabled JIT, small improvement in docker, native it gives me about 30% boost.

Optimisations by sqonk:
Changing SPLFixedArray to a simple string yields a nearly 2x performance increase.

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
