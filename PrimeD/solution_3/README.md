# D solution by @serg-gini

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Contributors:
- Michael Barber @mike-barber https://www.github.com/mike-barber -- original author
- Kai Rese @Kulasko https://github.com/Kulasko -- numerous idiomatic improvements and detailed code review
- @GordonBGood -- for plenty of collaboration on the `unrolled-hybrid` and `extreme-hybrid` solutions; check out his solutions, including Nim, Chapel, Haskell and Julia.
- OpenCode AI agent for code port to D (asked by Serg Gini)

# Configuration

The solution was tested with [LDC](https://github.com/ldc-developers/ldc) - LLVM based D compiler.

I've enabled all the optimisation I'm aware of, including:
- setting `-mcpu=native`
- link time optimisation and `noboundscheck`
- pay attention to [./dub.json](./dub.json)

# Quick start for those interested in D

Install D. It's really easy: https://tour.dlang.org. On Linux, it's just `curl -fsS https://dlang.org/install.sh | bash -s ldc`; on Mac `brew install ldc`. On Windows, just grab the installer from the link.

There is a convenient [run.sh](run.sh) file that builds and runs the solution.

Alternately, in the directory containing this README file, 
- run tests: `dub test` 
- build: `dub build --compiler=ldc2 --build=release`
- run: `./prime-sieve-d` or `dub run --build=release`
    - for help with command line parameters: `prime-sieve-d --help`
    - this allows you to specify sieve size, threads, etc.

To play with the code, the simplest approach is to use *Visual Studio Code* and install the `code-d` plugin.

And if you want to learn more, there are tons of great resources, including a good introductory book, on https://dlang.org/book/

## Output

Same structure as in original @mike-barber code:
```
serg-gini_bit-rotate;3887;5.0001104000;1;algorithm=base,faithful=yes,bits=1
serg-gini_bit-unrolled-hybrid;81511;5.0000557000;1;algorithm=base,faithful=yes,bits=1
serg-gini_bit-extreme-hybrid;82248;5.0000473000;1;algorithm=base,faithful=yes,bits=1
serg-gini_bit-rotate;14457;5.0011520000;4;algorithm=base,faithful=yes,bits=1
serg-gini_bit-unrolled-hybrid;187349;5.0017562000;4;algorithm=base,faithful=yes,bits=1
serg-gini_bit-extreme-hybrid;192654;5.0012310000;4;algorithm=base,faithful=yes,bits=1
serg-gini_bit-rotate;28630;5.0019960000;8;algorithm=base,faithful=yes,bits=1
serg-gini_bit-unrolled-hybrid;222008;5.0377162000;8;algorithm=base,faithful=yes,bits=1
serg-gini_bit-extreme-hybrid;243425;5.0026110000;8;algorithm=base,faithful=yes,bits=1
```

## Docker

You can also run the solution using Docker, without installing D.

```
./build-docker.sh
./run-docker.sh
```

For available command line options, just do `./run-docker.sh --help`

## Reference

The code and Readme were ported from great work of Michael Barber.
The algorithm [description](https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/PrimeRust/solution_1/README.md)
All credits to the algorithm authors.
This port is just showing that the algorithm could be more important than the language its implemented.
