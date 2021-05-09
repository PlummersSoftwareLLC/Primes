# Fortran solution by johandweber

![Category](https://img.shields.io/badge/Category-unfaithul-yellowgreen) 

I own the copyright to all code and am willing to license the code under BSD-3.

This a Fortran 2003 implementation of the sieve of Erathostenes algorithm.
It does not have external dependencies.
The Fortran language does not contain a bitfiled element, but allows the bitwise manipulation
of integers. Therefore 64-bit integers are used as store for the bitfield.

It is an unfaithful implementation as the sieve is not encapsulated within 
an object.However, Fortran 2003 (or later) in principle allows for a object 
oriented approach to the problem (I may implement it later).


##Run instructions 
The source code should be able to be compiled with every compiler
conforming to the Fortran 2003 standard. 
One example is gfortran, which is part of the GCC Compiler Collection
and available for all relevant desktop or server operating systems.

Assuming this comiler is used, the program can be compiled using the
command 

`gfortran -O3 -march=native -o prime PrimeFortran.f03` 

Here `-O3` is an optimization flag and `-march=native` tells the
compiler to optimize for the Processor on the system where the program is
compiled.

The program can then be executed normally, i.e.,

`./prime` 

when using a UNIX-like command line or just

`prime`

when using a DOS-like command line.


For the output of the results the switch `.false.` in the call of
`print_results` (line 60) has to be changed to `.true.`.
(Note: In Fortran the periods before and after `true` and `false`
are part of the boolean constants and must not be omitted.)

###Dockerfile

A Dockerfile is provided.



##Output 
johandweber_fortran;2739;5.002;1

The results seem to be strongly dependent on the optimizer.
The results are for gfortran 7.5. For the same options gfortran 9.3 has almost
double the number of iterations on the same machine.

For comparison: On my machine (AMD Ryzen 7 3700X) the PrimeCPP reaches 10716
iterations. When I remove a part of the output routines I can get similar results
with gfortran, but as I am not sure that in this case something is "optimized away",
this is not my "official" result.

