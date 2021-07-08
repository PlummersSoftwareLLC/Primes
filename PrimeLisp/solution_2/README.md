# Lisp solution by mayerrobert

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Based on the solution by mikehw.

## Run instructions

`sbcl --script PrimeSieve.lisp` will compile and run the program in one step.

## Output

    D:\robert\projects\primes\PrimeLisp\solution_2>sbcl --script PrimeSieve.lisp
    Passes: 1853  Time: 5.002 Avg: 2.6994064 ms Count: 78498
    mayerrobert-cl;1853;5.002;1;algorithm=base,faithful=no,bits=1

or if you want the CSV output only:

    D:\projects\primes\PrimeLisp\solution_2>sbcl --script PrimeSieve.lisp 2>nul
    mayerrobert-cl;1824;5.002;1;algorithm=base,faithful=no,bits=1

Unix users please adjust accordingly.
