# x86-64 assembly solution by rbergen, modified by joonicks

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This solution contains one implementations in x86-64 assembly:
* x64ff_unroll4, derived from solution_1/prime_ff_bitshift.asm

The basic algorithm used in all of these is that of the original C#/C++ implementations.

## Run instructions

### NASM/GCC
Execute the following command from the implementation directory, after NASM and GCC have been installed:
```
. build.sh
. run.sh
```

### Docker
A Dockerfile has been provided.

## Output
```
joonicks_x64ff_unroll4;4570;5.000;1;algorithm=base,faithful=yes,bits=1
```
