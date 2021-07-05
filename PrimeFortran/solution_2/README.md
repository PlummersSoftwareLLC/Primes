# Fortran solution by tjol

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)


This Fortran solution uses a Fortran 2003 class, unlike solution 1 by johandweber.
There are three versions:

 * `prime-bitarray`, the most faithful with 1 bit per flag and manual bit
   manipulation. ![Bit count](https://img.shields.io/badge/Bits-1-green)
 * `prime-8bit`, the fastest with an 8 bit integer per flag. ![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
 * `prime-logical-array`, which uses an array of `logical`. ![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Run instructions

    make run

## Output

    tjol-bits;9478;5.00024557;1;algorithm=base,faithful=yes,bits=1
    tjol-8bit;15241;5.00017738;1;algorithm=base,faithful=yes,bits=8
    tjol-logical;8691;5.00012016;1;algorithm=base,faithful=yes,bits=unknown

