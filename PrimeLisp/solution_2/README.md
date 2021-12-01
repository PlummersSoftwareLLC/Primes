# Common Lisp solutions by mayerrobert

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

`PrimeSieve.lisp` is based on the solution by mikehw,
with lots of type declarations to allow sbcl's optimizer do it's thing,
as well as manual loop unrolling by 4.
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

---

`PrimeSievebitops.lisp` doesn't use a bit vector but a machine word array and some bit operations to get and set bits.
Algorithm is base with 0 for primes.

It uses dense bit setting loops based on the ideas in https://github.com/PlummersSoftwareLLC/Primes/pull/680 .
The generated cond form that contains the dense bit-setting-loops for low factors
will be printed to the screen after uncommenting the last line.

`PrimeSievebitops.lisp` also amends sbcl's peephole optimizer with a pattern that combines successive `OR` instructions
with the same target register and immediate source operands of size signed-byte-32 or smaller.

For Common Lisp bit ops see https://lispcookbook.github.io/cl-cookbook/numbers.html#bit-wise-operation

---

`PrimeSieveModulo.lisp` (I think) uses the extreme loop unrolling method
as explained in https://github.com/PlummersSoftwareLLC/Primes/pull/641 .
The macro `generate-ecase` expands into an ecase form that contains specialized bit-setting-loops
for all combinations of start%8 and skip%8.
The generated ecase form will be printed to the screen after uncommenting the last line.


`PrimeSieveModuloFuncs.lisp` is similar to `PrimeSieveModulo.lisp` except instead of an ecase form
with unrolled loops a vector of functions is used where each function contains the unrolled loop.
These functions are generated and assigned to the vector by the function `generate-functions`.
The generated functions and assignments to the vector will be printed to the screen after uncommenting the last line.

Unfortunately sbcl misses some optimization opportunities
of `PrimeSieveModulo.lisp` and `PrimeSieveModuloFuncs.lisp` so that the performance is not as good as I was hoping.

---

`PrimeSieveWheelOpt.lisp` is a Common Lisp port of sieve_5760of30030_only_write_read_bits.c
by Daniel Spangberg.

Algorithm is _wheel_, see PrimeC/solution_2/README.md for a better explanation than I would be able to give.

`PrimeSieveWheelOpt.lisp` stores bits in an array of `(unsigned-byte 64)`,
much like Daniel's code uses an array of `uint64_t` when compiled with `-DCOMPILE_64_BIT`.


`PrimeSieveWheelBitvector.lisp` is pretty much the same as `PrimeSieveWheelOpt.lisp`
expect it uses a builtin bitvector instead on manual bit-fiddling.

---

`PrimeSievewordops.lisp` sets multiple bits at a time by copying bitpatterns (that are shifted and masked appropriately)
into the word-array.

---

All: The state of the sieve is stored in a Lisp class.


## Run instructions

First install "Steel Bank Common Lisp", see http://www.sbcl.org/platform-table.html.

Windows users may instead want to go to https://github.com/sbcl/sbcl -> Actions -> Windows
and enter e.g. `branch:sbcl-2.1.7` into the "Filter workflow runs" textfield.
This will lead you to https://github.com/sbcl/sbcl/actions/runs/1182434888.
From there you can download a Windows installer for SBCL 2.1.8.

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
    mayerrobert-cl;3337;5.008;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-hashdot;162685090;5.008;1;algorithm=base,faithful=no,bits=1
    mayerrobert-cl-dense;5694;5.007;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-modulo;2985;5.008;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-modulo-functions;3296;5.008;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-wheel-bitvector;6880;5.007;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-opt;6036;5.007;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-words;4131;5.008;1;algorithm=other,faithful=yes,bits=1


Using the provided dockerfile with podman on Fedora33, Pentium(R) Dual Core T4300 @ 2.10GHz I get

    $ podman build -t lisp2 .
    $ podman run --rm lisp2 2> /dev/null
    mayerrobert-cl;3399;5.000176;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-hashdot;176592210;5.000176;1;algorithm=base,faithful=no,bits=1
    mayerrobert-cl-dense;5662;5.000177;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-modulo;2884;5.001176;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-modulo-functions;3204;5.000177;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-wheel-bitvector;7173;5.000176;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-opt;6217;5.000177;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-words;4779;5.000183;1;algorithm=other,faithful=yes,bits=1

---

Using sbcl 2.1.8 on Windows 10, 11th Gen Intel(R) Core(TM) i5-1135G7 @ 2.40GHz (max turbo frequency 4.2 GHz) I get

    X:\projects\Primes\PrimeLisp\solution_2>run.cmd 2>nul
    mayerrobert-cl;9374;5.000022;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-hashdot;274647921;5.000001;1;algorithm=base,faithful=no,bits=1
    mayerrobert-cl-dense;18231;5.000097;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-modulo;4424;5.001007;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-modulo-functions;9184;5.000517;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-wheel-bitvector;24604;5.000063;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-opt;14935;5.000027;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-words;12952;5.000072;1;algorithm=other,faithful=yes,bits=1


Using sbcl 2.1.8 on Windows 10/ WSL2/ debian 10.9, 11th Gen Intel(R) Core(TM) i5-1135G7 @ 2.40GHz (max turbo frequency 4.2 GHz) I get

    $ sh run.sh 2> /dev/null
    mayerrobert-cl;9490;5.01;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-hashdot;553299588;5.01;1;algorithm=base,faithful=no,bits=1
    mayerrobert-cl-dense;18750;5.01;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-modulo;4477;5.01;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-modulo-functions;9460;5.01;1;algorithm=base,faithful=yes,bits=1
    mayerrobert-cl-wheel-bitvector;17870;5.01;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-wheel-opt;16053;5.01;1;algorithm=wheel,faithful=yes,bits=1
    mayerrobert-cl-words;14025;5.01;1;algorithm=other,faithful=yes,bits=1
