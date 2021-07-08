# Ada solution by heharkon

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Same implementation as with BoopBeepBoopBeep solution_1, but build parameters changed to use

"-gnatp", "-funroll-loops", "-O3"

to enable the optimizations (instead of debug build) and loop unrolling. Also runtime checks are omitted.

Using the Community IDE and Compiler found :
https://www.adacore.com/download
