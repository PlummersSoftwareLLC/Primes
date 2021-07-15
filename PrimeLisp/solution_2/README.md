# Common Lisp solutions by mayerrobert

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

`PrimeSieve.lisp` is based on the solution by mikehw,
with lots of type declarations to allow sbcl's optimizer do it's thing.
Algorithm is base.

Uses a bit-vector (one-dimensional arrays are called vector in Lisp)
for storage.

`PrimeSieveWheel.lisp` is a Common Lisp port of sieve_5760of30030_only_write_read_bits.c
by Daniel Spangberg.

The state of the sieve is stored in a Lisp struct (closest to a class in Lisp).

Algorithm is _wheel_, see PrimeC/solution_2/README.md for a better explanation than I would be able to give.

PrimeSieveWheel.lisp stores bits in an array of `(unsigned-byte 32)`,
much like Daniel's code uses an array of `uint32_t` when compiled without `-DCOMPILE_64_BIT`.

## Run instructions

First install "Steel Bank Common Lisp", see http://www.sbcl.org/platform-table.html.
Other Common Lisps should work as well ("Armed Bear Common Lisp" was lightly tested).

Then
`sbcl --script PrimeSieveWheel.lisp` will compile and run the program in one step,
`sbcl --script PrimeSieveWheel.lisp 2>nul` (Windows) or
`sbcl --script PrimeSieveWheel.lisp 2> /dev/null` (Unix)
will do the same but only output the CSV result data.

Or use `docker` or `podman` to build and run the provided dockerfile:

    $ cd PrimeLisp/solution_2
    $ podman build -t lisp2 .
    $ podman run lisp2

## Output

Using sbcl 2.0.0 on Windows 7, Pentium(R) Dual Core T4300 @ 2.10GHz I get

    D:\projects\primes\PrimeLisp\solution_2>sbcl --script PrimeSieve.lisp
    Passes: 1853  Time: 5.002 Avg: 2.6994064 ms Count: 78498
    mayerrobert-cl;1853;5.002;1;algorithm=base,faithful=no,bits=1
    
    D:\projects\Primes\PrimeLisp\solution_2>sbcl --script PrimeSieveWheel.lisp
    Passes: 4875  Time: 5.0 Avg: 1.0256411 ms Count: 78498
    mayerrobert-cl-wheel;4875;5.0;1;algorithm=wheel,faithful=no,bits=1
