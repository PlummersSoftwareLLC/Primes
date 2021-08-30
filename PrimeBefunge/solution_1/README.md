# Befunge-98 solution by tjol

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation of the prime sieve in in Befunge, specifically
Befunge-98.

Befunge is an esoteric programming language that frees you from the mundane
limitation of mostly unidirectional interpretation: Befunge programs are
two-dimensional, and the instruction pointer can move in any of the four
compass directions (and more!). Every character is a single instruction.

Data is kept either on a stack or in funge-space, which is also where the
program code lives. There is no separation between executable code and data,
and the program can freely modify itself if it so desires.

Since every instruction is a single character, numerical constants can be a bit
tricky to work with: you have to build numbers from the hexadecimal instructions
`0`..`f`, which push the numbers 0 to 15 onto the stack, and instructions like
`+`, `-`, `*`, `/` and `:` (duplicate). For example, 100 can be expressed as
`aa*`, 1000000 can be expressed as `aaaaaa*****`, `aaa**:*` or `a:*::**`, and
78498, in this program, looks like `9a*1-2*73*:**`.

This program, `primes.b98`, keeps the prime sieve state in the first line
(once the code there is no longer needed) and keeps a few other variables around
in the north-east corner. There's some self-modification around line 16/17,
column 80/81, where bits of the output code are used multiple times.

The additional program `primes-print1000.b98` loads the source code for
`primes.b98` and modifies it to print the primes below 1000 instead of running
the benchmark up to 1000000.

There are a number of built-in extensions known as ‘fingerprints’ with optional
instructions. This program uses three: `"BOOL"`, for bit manipulation, `"FIXP"`,
for the square root function, and `"HRTI"`, the high-resolution timer interface,
for the benchmark.

## Run Instructions

This program is known to work with  [cfunge](https://github.com/VorpalBlade/cfunge)
(latest git), [CCBI](https://github.com/Deewiant/CCBI),
[rfunge](https://github.com/tjol/rfunge), [Rc/Funge-98](https://rcfunge98.com) (V2),
and [Pyfunge](https://pypi.org/project/PyFunge/#files), though the latter two are
rather slow and cfunge is easily the fastest of the lot. If you have cfunge installed,
run

    cfunge primes.b98

from the source directory for the benchmark, or

    cfunge primes-print1000.b98

to print the primes below 1000.

## Ouput

    78498 primes: valid
    tjol-bf98;6;5.794812;1;algorithm=base;faithful=no;bits=1

