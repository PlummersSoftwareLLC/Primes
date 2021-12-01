# amd64 solution by dacvs (https://dacvs.neocities.org/)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This is a faithful implementation of the base prime number sieve algorithm from Dave Plummer's Software Drag Race.
This submission is written in machine language (amd64) in the style of my video series "Handmade Linux x86 executables."
(https://www.youtube.com/playlist?list=PLZCIHSjpQ12woLj0sjsnqDH8yVuXwTy3p).

When I open a source file (dacvs1.dmp or dacvs8.dmp) in vi (really vim) on my Ubuntu system, the text editor provides "syntax highlighting" by graying out the comments. This requires no special configuration. 

## Run instructions

To make the binary, run `make.sh` (on a Linux system).
To execute the binaries (after they are made) for primes up to 1 million, run `run.sh`.

## Output of run.sh
```
dacvs1;5581;5.000286;1;algorithm=base,faithful=yes,bits=1
dacvs8;7021;5.000677;1;algorithm=base,faithful=yes,bits=8
```

## Output of run1p.sh
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
I 5315
T 5000700682
1062.9
dacvs1;5315;5.000701;1;algorithm=base,faithful=yes,bits=1
```

## Output of run8p.sh
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

## This submission is faithful.

### External dependencies:
None.

### Class to encapsulate the sieve:
There are no classes in machine language. Why should a prime-sieve algorithm be object-oriented? Still, to be faithful, the amd64 submission does implement something like objects, and I suppose this is what assembly-language programmers would do.

See label Mark in file dacvs8.dmp. The sieve object consists of the upper bound M (e.g. M = 1000000) on the primes to be put out, represented as a 64-bit integer, followed immediately by an array for the sieve. Routine Mark writes M into the object, then sets variable A (a.k.a. A.r14) to point to the array. (Note, no special effort is required to initialize the array contents when the sieve object is constructed, as the memory system provides zeroed bytes, and my algorithm uses 0 for prime, 1 for composite.) After variable A is pointed to the array, there is no pointer to the object, but A still serves that purpose, as later (see label Obj) A is adjusted to point to the object just before the object is destroyed.

A new object is created each time through the loop. However, I arranged a memory "library" that reuses memory when the requested chunk is the same size as the last one destroyed. In this drag race, where memory is allocated dynamically and quickly freed (many times per second), this "library" feature prevents the program from making a syscall for each subsequent memory request. Many languages do some kind of memory management beyond what is offered by system calls. When making system calls by the interrupt instruction INT, a thousand syscalls per second is a lot, but perhaps such a load is no big deal with instruction SYSCALL and a modern Linux kernel. Anyway, the sieve is not reused. I explicitly clear the whole reused chunk of memory by instruction REP STOS under label Calloc.

### The sieve size and corresponding prime candidate memory buffer are set and allocated dynamically at runtime.
Yes, although not every request for memory triggers a system call, as described above.

### The size of the memory buffer must correspond to the size of the sieve.
Yes, in the 8-bit program, the buffer has 8 bits for each sieve number 1, 3, 5, 7, 9, ..., (M-1)/2.

The 1-bit program has 1 bit for each sieve number 1, 3, 5, 7, 9, ..., (M-1)/2, but the number of sieve bits is rounded up to the next multiple of 64, as sieve bits are accessed as members of 64-bit integers.

## This submission conforms to the rules:
### Solution uses sieve of Eratosthenes.
Yes.

### Benchmarked code returns a list of primes.
Script run.sh puts out one line of statistics in the preferred style for each program, 1-bit and 8-bit. Scripts run1p.sh and run8p.sh each put out a list of primes to stderr and a single line of statistics to stdout.

### Solution runs for at least 5 seconds, then stops as quickly as possible after that.
The algorithm completes its current pass before checking the clock, then stops if at least 5 seconds have passed. The program gets the time (down to the nanosecond) from Linux system call clock_gettime.

### Solution calculates all the primes up to 1000000.
Yes.

### You own copyright to all code and are willing to license that code under BSD-new.
Yes.

## Base algorithm:
### Search for next prime in the sieve, then clear this prime's multiples.
Yes, the program does this.
### When seeking factors, the algorithm sequentually checks all odd numbers, starting at 3.
Yes, the program does this.
### When clearing factors, the algorithm clears all non-primes individually, increasing the number by 2\*factor on each cycle.
Yes, the program does this.
### Not seeking factors beyond the square root of the sieve size is permissible.
Yes, the program stops seeking there.
### Inverted prime marking is permissible.
Yes, the program uses 0 for prime and 1 for non-prime.
### Starting the clearing loop at factor\*factor is permissible.
Yes, the program starts clearing there.
