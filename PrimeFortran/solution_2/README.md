# Fortran solution by tjol

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This Fortran solution uses a Fortran 2003 class, unlike solution 1 by johandweber.
There are three versions:

 * `prime-bitarray`, the most faithful with 1 bit per flag and manual bit
   manipulation. 
 * `prime-8bit`, the fastest with an 8 bit integer per flag. 
 * `prime-logical-array`, which uses an array of `logical`.

## Run instructions

    make run

## Output

    tjol-bits;10847;5.00003624;1;algorithm=base,faithful=yes,bits=1
    tjol-8bit;15269;5.00031376;1;algorithm=base,faithful=yes,bits=8
    tjol-logical;10110;5.00005150;1;algorithm=base,faithful=yes,bits=unknown
