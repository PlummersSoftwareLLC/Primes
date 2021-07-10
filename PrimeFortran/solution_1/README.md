# Fortran solution by johandweber

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

The copyright to the code is owned by Thomas Jollans (tjol) and me and we are 
willing to license the code under BSD-3.

This a Fortran 2008 implementation of the sieve of Erathostenes algorithm.
It does not have external dependencies.
The Fortran language does not contain a bitfiled element, but allows the bitwise manipulation
of integers. Therefore 8-bit integers are used as store for the bitfield.

It is an unfaithful implementation as the sieve is not encapsulated within 
an object.
In contrast to the previous version, the memory of the sieve is allocated and deallocated for each run.


## Run instructions 
The source code should be able to be compiled with every compiler
conforming to the Fortran 2008 standard. 
One example is gfortran, which is part of the GCC Compiler Collection
and available for all relevant desktop or server operating systems.

Assuming this compiler is used, the program can be compiled using the
command 

`gfortran -Ofast -march=native -o prime PrimeFortran.f03` 

Here `-Ofast` is an optimization flag and `-march=native` tells the
compiler to optimize for the Processor on the system where the program is
compiled.

The program can then be executed normally, i.e.,

`./prime` 

when using a UNIX-like command line or just

`prime`

when using a DOS-like command line.


For the output of the results the switch `.false.` in the call of
`print_results` (line 56) has to be changed to `.true.`.
(Note: In Fortran the periods before and after `true` and `false`
are part of the boolean constants and must not be omitted.)

## Dockerfile

A Dockerfile is provided.

## Output 

`johandweber_fortran;11123;5.000;1;algorithm=base,faithful=no,bits=1`



