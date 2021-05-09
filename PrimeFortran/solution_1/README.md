# Fortran solution by johandweber

![Category](https://img.shields.io/badge/Category-unfaithul-yellowgreen) 

I own the copyright to all code and am willing to license the code under BSD-3.

This a Fortran 2003 implementation of the sieve of Ersathostenes algorithm.
It does not have external dependencies.
Fortran does not contain a bitfiled element, but allows the bitwise manipulation
of integers. Thus use 64-bit integers as store for the bitfield.

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

`gfortran -O3 -march=native -o PrimeFortran PrimeFortran.f03` 

Here `-O3` is an optimization flag and `-march=native` tells the
compiler to optimize for the Processor on the system where the program is
compiled.

The program can then be executed normally, i.e.,

`./PrimeFortran <upper_limit>  <runtime in s> <show_output>` 

when using a UNIX-like command line or just

`PrimeFortran  <upper_limit>  <runtime in s> <show_output>`

when using a DOS-like command line.


Here the upper limit is an integer that defines the range to which the 
number of primes is determined , the runtime is a floating point number
that describes the runtime in seconds and. 
Using `T` as the third argument shows the found primes, `F` shows just
the performance data.

For example, a the sieve is computed with an upper limit of 100000 for
5 seconds without the input of the values by

`./PrimeFortran 1000000 5.0 F`

###Dockerfile

A Dockerfile is provided



##Output 

