# D solution by BradleyChatha

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A faithful yet more idiomatic and memory efficient (bits 1) implementation than solution_1, which appears to be a very direct translation of the original Python code.

This implementation is well commented on the D specific parts.

## Run instructions

### Without Docker

To run this without Docker, please install one of the [three D compilers](https://dlang.org/download.html).

These installations will come with a tool called [dub](https://dub.pm/getting_started) which is D's standard package manager & build tool.

You can then run any of the following commands, depending on your needs:

```
dub run                     # Runs a debug build using the default D compiler
dub run --compiler=ldc2     # Runs using a specific compiler
dub run -b release          # Runs a release build
```

To run unittests in D (this solution only has a single one) you can use:

```
dub test
dub test --compiler=...
```

## Output

Windows 10 Build 19041 | i5-7600K @ 3.80 GHz

```
<command> dub run -b release --compiler=ldc2
<stderr>  Passes: 6936, Time: 5 secs, 1 ms, 505 ╬╝s, and 9 hnsecs, Avg: 721 ╬╝s, Limit: 1000000, Count: 78498, Valid: false
<stderr>  Passes: 17101, Time: 5 secs, 169 ╬╝s, and 3 hnsecs, Avg: 292 ╬╝s and 3 hnsecs, Limit: 1000000, Count: 78498, Valid: false

BradleyChatha;6936;5.00151;1;algorithm=base,faithful=yes,bits=1
BradleyChatha-Multi;17101;5.00017;4;algorithm=base,faithful=yes,bits=1
```
