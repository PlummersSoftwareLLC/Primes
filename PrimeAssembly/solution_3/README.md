# x86-64 assembly solution by weirddan455

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

* primes_ff_1bit_rol is a faithful single bit implementation using the rolling mask optimization from PrimesCPP/solution_1. I've also included a couple other minor optimizations over the existing assembly solutions. The bit array gets reset using 128 bit SIMD and clock_gettime is using the C wrapper instead of the syscall (the latter has since been merged into 2 of the implementations in solution_1)

* primes_uff_1bit_rol is nearly identical except it stores the bit array in the bss segment as opposed to calling malloc. This makes it unfaithful but does result in a performance improvement.

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
weirddan455;5757;5.000453;1;algorithm=base,faithful=yes,bits=1
weirddan455;6046;5.000629;1;algorithm=base,faithful=no,bits=1
```
