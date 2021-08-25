# BrainFuck solution by ThatAquarel

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)
![Deviation](https://img.shields.io/badge/Deviation-sievesize-blue)

This solution implements a sieve similar to the original python version in the BrainFuck language. The amazing (perhaps
even amusing) part of this language is that there are only eight commands available that each consist of a single
character: `<>+-[],.`. Of course, this means there is no possibility for classes and timer apis.
This is why it is **unfaithful**, due an external C++ file that handles the timing and the printing of the results.

Note that in this current implementation, the hardcoded sieve size is `1000`.
Larger than that, and it will take an eternity to complete one single run.
With all that said, this is not intended to be a practical solution. 

## Run instructions

### Docker

The straightforward solution is to run the provided dockerfile.

### Build

Builds have been tested using `clang++-12`, `cmake 3.19.4` and `GNU Make 4.1` in `ubuntu 18.04`.

Let's start by building the interpreter for the BrainFuck language. First, clone the source files and prepare a folder
for builds.

```shell
$ git clone https://github.com/ThatAquarel/BrainF-ck-Interpreter \
    && mkdir ./BrainF-ck-Interpreter/release/ \
    && cd ./BrainF-ck-Interpreter/release/
```

Build and install the output binary.

```shell
$ cmake -DCMAKE_BUILD_TYPE=Release .. \
    && make \
    && cp BrainF_ck_Interpreter /usr/local/bin/brainfuck
```

Then build the external C++ invoker `PrimeBrainFuck.cpp`.
```shell
$ clang++-12 PrimeBrainFuck.cpp -oPrimeBrainFuck
```

### Run
Finally, run with the command:
```shell
$ ./PrimeBrainFuck PrimeBrainFuck.b
```

## Output

```
$ ./PrimeBrainFuck ./PrimeBrainFuck.b
Passes: 1, Time: 52.359554, Avg: 52.359554, Limit: 1000, Count1: 168, Valid: 1

aquarel;1;52.359554;1;algorithm=base,faithful=no,parallel=no,bits=32
```

It even completed in a timely manner, for a language like this.
