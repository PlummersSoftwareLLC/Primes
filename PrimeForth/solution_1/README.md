# Forth Prime Sieve by tjol

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is a straightforward port of Dave's algorithm to Forth (tested with GForth).

The state of the sieve is stored in dictionary-allocated memory, and allocated,
initialized and deallocated on every iteration.

There are two versions:

 * `prime-bitarray.fs`, storing the state in a bit field
 * `prime-bytearray.fs`, storing every prime/not prime flag in a byte

`prime-bitarray` is closest to the original, `prime-bytearray.fs` is faster.

## How to run

    ./run.sh

## Output

gforth 0.7.3:

    tjol-1bit;173 ;5.018379 ;1;algorithm=base,faithful=no,bits=1
    tjol-8bit;265 ;5.001464 ;1;algorithm=base,faithful=no,bits=8

gforth 0.7.9_20210701:
    
    tjol-1bit;465 ;5.002018 ;1;algorithm=base,faithful=no,bits=1
    tjol-8bit;475 ;5.000922 ;1;algorithm=base,faithful=no,bits=8

gforth 0.7.9_20190627 in the Docker container

    tjol-1bit;256 ;5.001076 ;1;algorithm=base,faithful=no,bits=1
    tjol-8bit;418 ;5.0083 ;1;algorithm=base,faithful=no,bits=8

This is on a Ryzen 5 2600 running OpenSUSE Tumbleweed.
