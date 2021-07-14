# D solution by Bradley Chatha, Paul Backus, Bastiaan Veelo

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

A faithful yet more idiomatic and memory efficient (bits 1) implementation than solution_1, which appears to be a very direct translation of the original Python code.

This implementation is well commented on the D specific parts.

It includes an unfaithful version (due to certain things not being runtime values) and a faithful version. Both are ran in a single threaded and then
multi-threaded test.

If you're not familiar with some of the antics D can achieve... have fun with the code.

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
<stderr> Passes: 7620, Time: 5 secs, 487 μs, and 6 hnsecs, Avg: 656 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
<stderr> Passes: 24501, Time: 5 secs, 98 μs, and 9 hnsecs, Avg: 204 μs, Limit: 1000000, Count: 78498, Valid: true
<stderr> Passes: 7523, Time: 5 secs, 42 μs, and 9 hnsecs, Avg: 664 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true
<stderr> Passes: 12349, Time: 5 secs, 1 ms, 12 μs, and 8 hnsecs, Avg: 404 μs and 9 hnsecs, Limit: 1000000, Count: 78498, Valid: true

BradleyChatha-Single-SieveCT;7620;5.00049;1;algorithm=base,bits=1,faithful=no
BradleyChatha-Multi-SieveCT;24501;5.0001;4;algorithm=base,bits=1,faithful=no
BradleyChatha-Single-SieveRT;7523;5.00004;1;algorithm=base,bits=1,faithful=yes
BradleyChatha-Multi-SieveRT;12349;5.00101;4;algorithm=base,bits=1,faithful=yes
```
