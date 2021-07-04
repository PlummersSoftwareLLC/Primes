# Forth Prime Sieve by tjol

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is a straighforward port of Dave's algorithm to Forth (tested with GForth).

The state of the sieve is stored in dictionary-allocated memory, and allocated,
initialized and deallocated on every iteration.

There are two versions:

 * `prime-bitarray.fs`, storing the state in a bit field
 * `prime-bytearray.fs`, storing every prime/not prime flag in a byte

`prime-bitarray` is closest to the original, `prime-bytearray.fs` is faster. The docker
container runs `prime-bitarray.fs`.

## How to run

The program doesn't have a convenient command-line interface.

 * For the 5 second benchmark: `gforth-fast prime-bitarray.fs -e "5 1000000 print-benchmark-results bye"`
 * To print the primes up to (e.g.) 1000: `gforth-fast prime-bitarray.fs -e "1000 print-primes bye"`
 * To check the result for a sieve size of (e.g.) 100'000'000: `gforth-fast prime-bitarray.fs -e "100000000 run-validation bye"`

## Output

Bit-array version, gforth 0.7.3:

    tjol-1bit;175 ;5.020695 ;1;algorithm=base,faithful=no

Byte-array version, gforth 0.7.3:

    tjol-8bit;284 ;5.00106 ;1;algorithm=base,faithful=no

Bit-array version, gforth 0.7.9_20210701:
    
    tjol-1bit;426 ;5.000673 ;1;algorithm=base,faithful=no

Byte-array version, gforth 0.7.9_20210701:
    
    tjol-8bit;481 ;5.008582 ;1;algorithm=base,faithful=no

Bit-array version in docker, gforth 0.7.9_20190627:

    tjol-1bit;259 ;5.000519 ;1;algorithm=base,faithful=no

This is on a Ryzen 5 2600 running OpenSUSE Tumbleweed.
