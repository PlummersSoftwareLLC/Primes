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

### Docker
A Dockerfile has been provided.

## Output
```
NotMatthewGriffin_SML;1252;5.003;1;algorithm=base,faithful=yes,bits=1
```
