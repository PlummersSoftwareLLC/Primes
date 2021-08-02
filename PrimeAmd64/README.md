# amd64 solution by @dacvs (https://dacvs.neocities.org/)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This is an 8-bit faithful implementation of the base prime number sieve algorithm from Dave Plummer's Software Drag Race.
This submission is written in machine language (amd64) in the style of my video series "Handmade Linux x86 executables."
(https://www.youtube.com/playlist?list=PLZCIHSjpQ12woLj0sjsnqDH8yVuXwTy3p).

When I open the source file dacvs8.dmp in vi (really vim) on my Ubuntu system, the text editor provides "syntax highlighting" by graying out the comments. This requires no special configuration. 

## Run instructions

To make the binary, run `make.sh` (on a Linux system).
To execute the binary (after it is made) for primes up to 1 million, run `run.sh`.

## Output

```
P 2
P 3
P 5
P 7
P 11
P 13
P 17
P 19
P 23
P 29
...
P 999953
P 999959
P 999961
P 999979
P 999983
= 78498
I 6383
T 5000102361
1276.6
dsmith8;6383;5.000102;1;algorithm=base,faithful=yes,bits=8

```
