# Standard ML solution by NotMatthewGriffin

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation in Standard ML.

## Run instructions

### SMLofNJ

Execute the following from this directory:

```bash
sml build.sml
sml @SMLload=primes.x86-linux
```

### MLTON

Execute the following from this directory:

```bash
mlton -link-opt '-static' primes.mlb
./primes
```

### MLTON with llvm

With a recent enough version of the mlton compiler (since release 20180207) llvm can be used for code generation. If the tools `llvm-as`, `opt`, `llc` are available in the compilation environment then this option can be used to compile a faster sieve program.

Execute the following from this directory:

```
mlton -link-opt '-static' -codegen llvm primes.mlb
./primes
```

### Docker
A Dockerfile has been provided.

## Output
```
NotMatthewGriffin_SML;2181;5.001;1;algorithm=base,faithful=yes,bits=1
```
