# D solution by Bradley Chatha, Paul Backus, Bastiaan Veelo

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-lookup-yellowgreen)
![Algorithm](https://img.shields.io/badge/Algorithm-pregenerated-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-0-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Deviation](https://img.shields.io/badge/Deviation-compile%20time-blue)

Several solutions are provided, which are listed in the table below.

This implementation is well commented on the D specific parts.

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

<!--MDGEN_START-->
## Solutions

| Tag | Description | Multithreaded | Passes | Algorithm | Bits | Faithful |
|-----|-------------|---------------|--------|-----------|------|----------|
| SieveRT | Sieve that where everything is allocated and computed at runtime. | staticThreads | 24574 | base | 1 | true |
| SieveRT_8 | An 8-bit variant of SieveRT. | staticThreads | 21569 | base | 8 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | dynamicThreads | 13629 | base | 1 | true |
| SieveRT_8 | An 8-bit variant of SieveRT. | dynamicThreads | 10097 | base | 8 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | single | 7003 | base | 1 | true |
| SieveRT_8 | An 8-bit variant of SieveRT. | single | 6181 | base | 8 | true |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | staticThreads | 323844955 | pregenerated | 0 | false |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | single | 94602080 | pregenerated | 0 | false |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | dynamicThreads | 4409977 | pregenerated | 0 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | staticThreads | 44017 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | staticThreads | 27679 | base | 1 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | dynamicThreads | 25313 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | dynamicThreads | 24821 | base | 1 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | single | 12121 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | single | 7691 | base | 1 | false |


## Output

```
Command: dub run -b release --compiler=ldc2
stderr:
    Passes: 7691, Time: 5 secs, 28 μs, and 5 hnsecs, Avg: 650 μs and 1 hnsec, Limit: 1000000, Count: 78498, Valid: true
    Passes: 24821, Time: 5 secs, 490 μs, and 2 hnsecs, Avg: 201 μs and 4 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 27679, Time: 5 secs, 668 μs, and 2 hnsecs, Avg: 180 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 7003, Time: 5 secs, 306 μs, and 5 hnsecs, Avg: 714 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 13629, Time: 5 secs, 6 ms, 845 μs, and 9 hnsecs, Avg: 367 μs and 3 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 24574, Time: 5 secs, 658 μs, and 1 hnsec, Avg: 203 μs and 4 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 94602080, Time: 5 secs and 1 hnsec, Avg: 0 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 4409977, Time: 5 secs, 17 μs, and 3 hnsecs, Avg: 1 μs and 1 hnsec, Limit: 1000000, Count: 78498, Valid: true
    Passes: 323844955, Time: 5 secs, 12 μs, and 8 hnsecs, Avg: 0 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 12121, Time: 5 secs, 292 μs, and 7 hnsecs, Avg: 412 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 25313, Time: 5 secs, 320 μs, and 5 hnsecs, Avg: 197 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 44017, Time: 5 secs and 254 μs, Avg: 113 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 6181, Time: 5 secs, 743 μs, and 4 hnsecs, Avg: 809 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 10097, Time: 5 secs, 55 μs, and 1 hnsec, Avg: 495 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 21569, Time: 5 secs, 673 μs, and 2 hnsecs, Avg: 231 μs and 8 hnsecs, Limit: 1000000, Count: 78498, Valid: true

stdout:
    BradleyChatha-Single-SieveCT;7691;5.00003;1;algorithm=base,bits=1,faithful=no
    BradleyChatha-MultidynamicThreads-SieveCT;24821;5.00049;4;algorithm=base,bits=1,faithful=no
    BradleyChatha-MultistaticThreads-SieveCT;27679;5.00067;4;algorithm=base,bits=1,faithful=no
    BradleyChatha-Single-SieveRT;7003;5.00031;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT;13629;5.00685;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT;24574;5.00066;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-Single-SieveRTCT_Cheatiness;94602080;5;1;algorithm=pregenerated,bits=0,faithful=no
    BradleyChatha-MultidynamicThreads-SieveRTCT_Cheatiness;4409977;5.00002;4;algorithm=pregenerated,bits=0,faithful=no
    BradleyChatha-MultistaticThreads-SieveRTCT_Cheatiness;323844955;5.00001;4;algorithm=pregenerated,bits=0,faithful=no
    BradleyChatha-Single-SieveRT_LookupTable;12121;5.00029;1;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-MultidynamicThreads-SieveRT_LookupTable;25313;5.00032;4;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-MultistaticThreads-SieveRT_LookupTable;44017;5.00025;4;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-Single-SieveRT_8;6181;5.00074;1;algorithm=base,bits=8,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT_8;10097;5.00006;4;algorithm=base,bits=8,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT_8;21569;5.00067;4;algorithm=base,bits=8,faithful=yes
```

