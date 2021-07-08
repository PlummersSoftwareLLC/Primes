# Common Lisp port of sieve_5760of30030_only_write_read_bits.c

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-2-yellowgreen)

Original C-code in `PrimeC/solution_2/sieve_5760of30030_only_write_read_bits.c`by Daniel Spangberg

Common Lisp port by mayerrobert

The state of the sieve is stored in a Lisp struct (closest to a class in Lisp).

Algorythm is _wheel_, see PrimeC/solution_2/README.md for a better explanation than I would be able to give.

PrimeLisp.lisp stores bits in an array of fixnums,
much like Daniel's code uses an array of uint32_t when compiled without -DCOMPILE_64_BIT.

Lisp's fixnum however is different compared to C-data types:
Lisp _values_ have type information (variables may have _optional_ type information),
and fixnums store this type information in a few bits of each value,
leaving (in case of 64bit sbcl) 62 usable bits.
PrimeSieve.lisp uses 32 of these usable bits to store it's data.

## Run instructions

`sbcl --script PrimeSieve.lisp` will compile and run the program in one step,
`sbcl --script PrimeSieve.lisp 2>nul` (Windows)
or `sbcl --script PrimeSieve.lisp 2> /dev/null`
will do the same but only output the CSV result data.

## Output

    D:\projects\primes\PrimeLisp\solution_3>sbcl --script PrimeSieve.lisp
    Passes: 4480  Time: 5.002 Avg: 1.1165179 ms Count: 78498
    mayerrobert-cl-wheel;4480;5.002;1;algorithm=wheel,faithful=no,bits=2
