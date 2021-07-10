# Lisp solution by mayerrobert

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Based on the solution by mikehw,
with lots of type declarations to help sbcl's optimizer.

## Run instructions

`sbcl --script PrimeSieve.lisp` will compile and run the program in one step,
`sbcl --script PrimeSieve.lisp 2>nul` (Windows)
or `sbcl --script PrimeSieve.lisp 2> /dev/null` (Unix)
will do the same but only output the CSV result data.

## Output

    D:\robert\projects\primes\PrimeLisp\solution_2>sbcl --script PrimeSieve.lisp
    Passes: 1853  Time: 5.002 Avg: 2.6994064 ms Count: 78498
    mayerrobert-cl;1853;5.002;1;algorithm=base,faithful=no,bits=1
