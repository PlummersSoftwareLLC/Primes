# Common Lisp port of PrimeC/solution_2/sieve_5760of30030_only_write_read_bits.c

Original C-code by Daniel Spangberg

Common Lisp port by mayerrobert

The state of the sieve is stored in a Lisp struct (closest to a class in C).

Algorythm is _wheel_, see PrimeC/solution_2/README.md for an explanation.

PrimeLisp.lisp stores bits in an array of fixnums,
much like Daniel's code uses an array of uint32_t when compiled without -DCOMPILE_64_BIT.

Lisp's fixnum however is different compared to C-data types:
Lisp _values_ have type information (variables may have _optional_ type information),
and fixnums store this type information in a few bits of each value,
leaving (in case of 64bit sbcl) 62 usable bits.
PrimeSieve.lisp uses 32 of these usable bits to store it's data.


![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-2-yellowgreen)
