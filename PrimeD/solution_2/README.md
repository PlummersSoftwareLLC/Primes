# D solution by Bradley Chatha, Paul Backus, Bastiaan Veelo

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-lookup-yellowgreen)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-0-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-16-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-64-yellowgreen)
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
| SieveRT | Sieve that where everything is allocated and computed at runtime. | staticThreads | 24760 | base | 1 | true |
| SieveRT_8 | An 8-bit variant of SieveRT. | staticThreads | 18442 | base | 8 | true |
| SieveRT_16 | None given for this solution. | staticThreads | 16971 | base | 16 | true |
| SieveRT_32 | None given for this solution. | staticThreads | 14831 | base | 32 | true |
| SieveRT_16 | None given for this solution. | dynamicThreads | 14017 | base | 16 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | dynamicThreads | 13389 | base | 1 | true |
| SieveRT_64 | None given for this solution. | staticThreads | 12061 | base | 64 | true |
| SieveRT_32 | None given for this solution. | dynamicThreads | 10341 | base | 32 | true |
| SieveRT_8 | An 8-bit variant of SieveRT. | dynamicThreads | 10185 | base | 8 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | single | 7001 | base | 1 | true |
| SieveRT_64 | None given for this solution. | dynamicThreads | 6613 | base | 64 | true |
| SieveRT_8 | An 8-bit variant of SieveRT. | single | 6340 | base | 8 | true |
| SieveRT_16 | None given for this solution. | single | 5647 | base | 1 | true |
| SieveRT_32 | None given for this solution. | single | 4642 | base | 1 | true |
| SieveRT_64 | None given for this solution. | single | 3985 | base | 1 | true |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | staticThreads | 322972998 | other | 0 | false |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | single | 94483313 | other | 0 | false |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | dynamicThreads | 4619129 | other | 0 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | staticThreads | 46699 | lookup | 1 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | dynamicThreads | 29029 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | staticThreads | 27653 | base | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | dynamicThreads | 24665 | base | 1 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | single | 12235 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | single | 7724 | base | 1 | false |


## Output

```
Command: dub run -b release --compiler=ldc2
stderr:
    Passes: 7724, Time: 5 secs, 71 μs, and 6 hnsecs, Avg: 647 μs and 3 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 24665, Time: 5 secs, 1 ms, 138 μs, and 7 hnsecs, Avg: 202 μs and 7 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 27653, Time: 5 secs, 523 μs, and 8 hnsecs, Avg: 180 μs and 8 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 7001, Time: 5 secs, 456 μs, and 7 hnsecs, Avg: 714 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 13389, Time: 5 secs, 1 ms, 670 μs, and 2 hnsecs, Avg: 373 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 24760, Time: 5 secs and 584 μs, Avg: 201 μs and 9 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 94483313, Time: 5 secs, Avg: 0 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 4619129, Time: 5 secs, 1 μs, and 4 hnsecs, Avg: 1 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 322972998, Time: 5 secs and 31 μs, Avg: 0 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 12235, Time: 5 secs, 25 μs, and 4 hnsecs, Avg: 408 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 29029, Time: 5 secs, 386 μs, and 6 hnsecs, Avg: 172 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 46699, Time: 5 secs, 188 μs, and 4 hnsecs, Avg: 107 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 6340, Time: 5 secs, 449 μs, and 8 hnsecs, Avg: 788 μs and 7 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 10185, Time: 5 secs, 555 μs, and 5 hnsecs, Avg: 490 μs and 9 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 18442, Time: 5 secs, 1 ms, 973 μs, and 8 hnsecs, Avg: 271 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 5647, Time: 5 secs, 48 μs, and 7 hnsecs, Avg: 885 μs and 4 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 14017, Time: 5 secs, 61 μs, and 3 hnsecs, Avg: 356 μs and 7 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 16971, Time: 5 secs, 1 ms, 380 μs, and 1 hnsec, Avg: 294 μs and 7 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 4642, Time: 5 secs, 920 μs, and 1 hnsec, Avg: 1 ms, 77 μs, and 3 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 10341, Time: 5 secs, 1 ms, 379 μs, and 4 hnsecs, Avg: 483 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 14831, Time: 5 secs, 1 ms, 740 μs, and 3 hnsecs, Avg: 337 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 3985, Time: 5 secs, 432 μs, and 6 hnsecs, Avg: 1 ms, 254 μs, and 8 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 6613, Time: 5 secs, 2 ms, and 271 μs, Avg: 756 μs and 4 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 12061, Time: 5 secs, 1 ms, 662 μs, and 6 hnsecs, Avg: 414 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true

stdout:
    BradleyChatha-Single-SieveCT;7724;5.00007;1;algorithm=base,bits=1,faithful=no
    BradleyChatha-MultidynamicThreads-SieveCT;24665;5.00114;4;algorithm=base,bits=1,faithful=no
    BradleyChatha-MultistaticThreads-SieveCT;27653;5.00052;4;algorithm=base,bits=1,faithful=no
    BradleyChatha-Single-SieveRT;7001;5.00046;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT;13389;5.00167;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT;24760;5.00058;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-Single-SieveRTCT_Cheatiness;94483313;5;1;algorithm=other,bits=0,faithful=no
    BradleyChatha-MultidynamicThreads-SieveRTCT_Cheatiness;4619129;5;4;algorithm=other,bits=0,faithful=no
    BradleyChatha-MultistaticThreads-SieveRTCT_Cheatiness;322972998;5.00003;4;algorithm=other,bits=0,faithful=no
    BradleyChatha-Single-SieveRT_LookupTable;12235;5.00003;1;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-MultidynamicThreads-SieveRT_LookupTable;29029;5.00039;4;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-MultistaticThreads-SieveRT_LookupTable;46699;5.00019;4;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-Single-SieveRT_8;6340;5.00045;1;algorithm=base,bits=8,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT_8;10185;5.00056;4;algorithm=base,bits=8,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT_8;18442;5.00197;4;algorithm=base,bits=8,faithful=yes
    BradleyChatha-Single-SieveRT_16;5647;5.00005;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT_16;14017;5.00006;4;algorithm=base,bits=16,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT_16;16971;5.00138;4;algorithm=base,bits=16,faithful=yes
    BradleyChatha-Single-SieveRT_32;4642;5.00092;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT_32;10341;5.00138;4;algorithm=base,bits=32,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT_32;14831;5.00174;4;algorithm=base,bits=32,faithful=yes
    BradleyChatha-Single-SieveRT_64;3985;5.00043;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT_64;6613;5.00227;4;algorithm=base,bits=64,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT_64;12061;5.00166;4;algorithm=base,bits=64,faithful=yes
```

