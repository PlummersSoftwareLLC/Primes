# Common Lisp solutions by mayerrobert

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

`PrimeSieve.lisp` is based on the solution by mikehw,
with lots of type declarations to allow sbcl's optimizer do it's thing.
Algorithm is base.

The state of the sieve is stored in a Lisp class.

Uses a bit-vector (one-dimensional arrays are called vector in Lisp)
for storage.

`PrimeSievebitops.lisp` doesn't use a bit vector but a machine word array and some bit operations to get and set bits.
The state of the sieve is stored in a Lisp class.

For Common Lisp bit ops see https://lispcookbook.github.io/cl-cookbook/numbers.html#bit-wise-operation
Also it uses inverted logic, i.e. 0 for primes.

`PrimeSievewordops.lisp` sets multiple bits at a time by copying bitpatterns (that are shifted and masked appropriately)
into the word-array.

`PrimeSieveWheel.lisp` is a Common Lisp port of sieve_5760of30030_only_write_read_bits.c
by Daniel Spangberg.

The state of the sieve is stored in a Lisp class.

Algorithm is _wheel_, see PrimeC/solution_2/README.md for a better explanation than I would be able to give.

PrimeSieveWheel.lisp stores bits in an array of `(unsigned-byte 64)`,
much like Daniel's code uses an array of `uint64_t` when compiled with `-DCOMPILE_64_BIT`.

`PrimeSieveWheelOpt.lisp` contains further performance improvements.
I think the biggest improvement is using FLOOR vs AND+SHIFT
because FLOOR does both in one step.

The state of the sieve is stored in a Lisp class.

Re: optimizations; (compile-file "PrimeSieveWheel.lisp") will show lots of info during the compilation
regarding inefficient code that can't be optimized.

## Run instructions

First install "Steel Bank Common Lisp", see http://www.sbcl.org/platform-table.html.
Other Common Lisps should work as well ("Armed Bear Common Lisp" was lightly tested).

Then
`sbcl --script PrimeSieveWheelOpt.lisp` will compile and run the program in one step,
`sbcl --script PrimeSieveWheelOpt.lisp 2>nul` (Windows) or
`sbcl --script PrimeSieveWheelOpt.lisp 2> /dev/null` (Unix)
will do the same but only output the CSV result data.

Or use `run.cmd` (Windows) or `./run.sh` (Unix) to run all files.

If you can't or won't install sbcl then use `docker` or `podman` to build and run the provided dockerfile:

    $ cd PrimeLisp/solution_2
    $ podman build -t lisp2 .
    $ podman run --rm lisp2

## Output

Using sbcl 2.0.0 on Windows 7, Pentium(R) Dual Core T4300 @ 2.10GHz I get

    D:\robert\projects\Primes\PrimeLisp\solution_2>run.cmd 2>nul
    mayerrobert-cl;1987;5.007;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-clb;2209;5.007;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-wheel;5490;5.007;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-opt;6026;5.008;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-words;4086;5.008;1;algorithm=base,faithful=yes,bits=1

Using sbcl 2.1.6 on Windows 10/ WSL2/ debian 10.9, 11th Gen Intel(R) Core(TM) i5-1135G7 @ 2.40GHz I get

    $ ./run.sh 2>/dev/null
    mayerrobert-cl;3610;5.0;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-wheel;16183;5.0;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-opt;16053;5.0;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-clb;5348;5.0;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-words;11692;5.0;1;algorithm=base,faithful=yes,bits=1
