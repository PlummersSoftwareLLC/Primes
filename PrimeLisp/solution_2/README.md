# Common Lisp solutions by mayerrobert

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

`PrimeSieve.lisp` is based on the solution by mikehw,
with lots of type declarations to allow sbcl's optimizer do it's thing.
Algorithm is base with 0 for primes.

Uses a bit-vector (one-dimensional arrays are called vector in Lisp)
for storage.

The timed loop is run twice: the first loop is run in a faithful fashion,
in the second loop the call to `run-sieve` is prepended with the go-fast operator `#.`
which results in a (cough) considerable speedup.

Explanation: sbcl is not a compiler but a Lisp system that contains
a Lisp-reader (the Lisp-reader also processes reader-macros),
a macro facility (for defmacro),
a compiler
and an evaluator.

When using the commandline parameter `--script`
sbcl will use it's Lisp-reader to read S-expressions from the given file
(S-expressions are Lisp's surface representation,
Lisp itself is not specified in terms of program text but in terms of in-memory structures).

After each top-level S-expression is read (during which reader macros are expanded)
it will be macroexpanded (note that this is another kind of macro-facility different from reader-macros!),
compiled and run.

I.e. there is *read-time* (which includes processing of reader-macros),
*macroexpansion-time*,
*compile-time* and
*evaluation-time*.

`#.` is processed during read-time and means "read-time evaluation",
i.e. the form following `#.` will be evaluated at read-time
and the result of this evaluation will replace the original form.

So, to summarize: `mayerrobert-cl-hashdot` does not use compile-time evaluation,
the evaluation happens *before* compile-time.


`PrimeSievebitops.lisp` doesn't use a bit vector but a machine word array and some bit operations to get and set bits.
Algorithm is base with 0 for primes.

For Common Lisp bit ops see https://lispcookbook.github.io/cl-cookbook/numbers.html#bit-wise-operation


`PrimeSievewordops.lisp` sets multiple bits at a time by copying bitpatterns (that are shifted and masked appropriately)
into the word-array.


`PrimeSieveWheel.lisp` is a Common Lisp port of sieve_5760of30030_only_write_read_bits.c
by Daniel Spangberg.

Algorithm is _wheel_, see PrimeC/solution_2/README.md for a better explanation than I would be able to give.

PrimeSieveWheel.lisp stores bits in an array of `(unsigned-byte 64)`,
much like Daniel's code uses an array of `uint64_t` when compiled with `-DCOMPILE_64_BIT`.


`PrimeSieveWheelOpt.lisp` contains further performance improvements.
I think the biggest improvement is using FLOOR vs AND+SHIFT
because FLOOR does both in one step.


`PrimeSieveWheelBitvector.lisp` is pretty much the same as `PrimeSieveWheelOpt.lisp`
expect it uses a builtin bitvector instead on manual bit-fiddling
because as of the upcoming SBCL 2.1.8 builtin bitvector operations are faster by a lot.


`PrimeSieve.lisp` and `PrimeSieveWheelBitvector.lisp` load code that patches SBCL on the fly
to get the faster bitvector-set operations from the not-yet-released SBCL 2.1.8, see `bitvector-set-*.lisp`.


All: The state of the sieve is stored in a Lisp class.


Re: optimizations; (compile-file "PrimeSieveWheel.lisp") will show lots of info during the compilation
regarding inefficient code that can't be optimized.

## Run instructions

First install "Steel Bank Common Lisp", see http://www.sbcl.org/platform-table.html.

Windows users may instead want to go to https://github.com/sbcl/sbcl -> Actions -> Windows
and enter e.g. `branch:sbcl-2.1.7` into the "Filter workflow runs" textfield.
This will lead you to https://github.com/sbcl/sbcl/actions/runs/1082323609.
From there you can download a Windows installer for SBCL 2.1.7.

Other Common Lisps should work as well ("Armed Bear Common Lisp" was lightly tested).

Then
`sbcl --script PrimeSieveWheelOpt.lisp` will compile and run the program in one step,
`sbcl --script PrimeSieveWheelOpt.lisp 2>nul` (Windows) or
`sbcl --script PrimeSieveWheelOpt.lisp 2> /dev/null` (Unix)
will do the same but only output the CSV result data.

Or use `run.cmd` (Windows) or `sh run.sh` (Unix) to run all files.

If you can't or won't install sbcl then use `docker` or `podman` to build and run the provided dockerfile:

    $ cd PrimeLisp/solution_2
    $ podman build -t lisp2 .
    $ podman run --rm lisp2

## Output

Using sbcl 2.0.0 on Windows 7, Pentium(R) Dual Core T4300 @ 2.10GHz I get

    D:\robert\projects\Primes\PrimeLisp\solution_2>run.cmd 2>nul
    mayerrobert-cl;3324;5.008;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-hashdot;160542746;5.007;1;algorithm=base,faithful=no,bits=1
    mayerrobert-clb;2412;5.008;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-wheel;5485;5.008;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-bitvector;6882;5.007;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-opt;6020;5.008;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-words;4022;5.007;1;algorithm=base,faithful=no,bits=1


Using the provided dockerfile with podman on Fedora33, Pentium(R) Dual Core T4300 @ 2.10GHz I get

    $ podman build -t lisp2 .
    $ podman run --rm lisp2 2> /dev/null
    mayerrobert-cl;3388;5.000176;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-hashdot;174503774;5.000176;1;algorithm=base,faithful=no,bits=1
    mayerrobert-cl-wheel;5716;5.000177;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-bitvector;7171;5.000176;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-opt;6213;5.000177;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-clb;2373;5.001176;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-words;4236;5.000176;1;algorithm=base,faithful=no,bits=1


Using sbcl 2.1.7 on Windows 10, 11th Gen Intel(R) Core(TM) i5-1135G7 @ 2.40GHz (max turbo frequency 4.2 GHz) I get

    mayerrobert-cl;9320;5.000174;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-hashdot;274020057;5.000001;1;algorithm=base,faithful=no,bits=1
    mayerrobert-clb;6130;5.000712;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-wheel;17894;5.000222;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-bitvector;24388;5.000181;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-opt;19478;5.000096;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-words;10882;5.000439;1;algorithm=base,faithful=no,bits=1


Using sbcl 2.1.7 on Windows 10/ WSL2/ debian 10.9, 11th Gen Intel(R) Core(TM) i5-1135G7 @ 2.40GHz (max turbo frequency 4.2 GHz) I get

    $ ./run.sh 2>/dev/null
    mayerrobert-cl;9504;5.01;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-hashdot;509110305;5.01;1;algorithm=base,faithful=no,bits=1
    mayerrobert-cl-wheel;15479;5.0;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-bitvector;19903;5.01;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-opt;16653;5.01;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-clb;6365;5.01;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-words;11552;5.01;1;algorithm=base,faithful=no,bits=1

