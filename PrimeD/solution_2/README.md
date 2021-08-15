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

You must specify a command line flag `-m` whose value is either `leaderboard` or `all` in order to select which sets to run.

## Run instructions

### Without Docker

To run this without Docker, please install one of the [three D compilers](https://dlang.org/download.html).

These installations will come with a tool called [dub](https://dub.pm/getting_started) which is D's standard package manager & build tool.

You can then run any of the following commands, depending on your needs:

```
dub run -- -m leaderboard                     # Runs a debug build using the default D compiler
dub run --compiler=ldc2 -- -m leaderboard     # Runs using a specific compiler
dub run -b release -- -m leaderboard          # Runs a release build
```

To run unittests in D (this solution only has a single one) you can use:

```
dub test
dub test --compiler=...
```

## Classifications

There are 3 test types currently: single, dynamicThreads, and staticThreads:

* **Single** - The test is ran in a single thread.
* **staticThreads** - The test is ran in multiple threads (cpu core count) where the threads are allocated once and run continuously.
* **dynamicThreads** - The test is ran in multiple threads where the threads are assigned (not allocated since I assume it uses a thread pool) every loop iteration.

In general "RT" means "Runtime" and "CT" means "Compile time". Compile time does not mean it runs completely at compile time, but only parts of it might.

## Graphs

The Y-axis refers to passes/second.

![dynamic faithful](https://i.imgur.com/5I57pMN.png)

![dynamic unfaithful](https://i.imgur.com/ntNgqsX.png)

![static faithful](https://i.imgur.com/4LUvgAZ.png)

![static unfaithful](https://i.imgur.com/4IuxHdf.png)

![single faithful](https://i.imgur.com/P11Z6zs.png)

![single unfaithful](https://i.imgur.com/YMa6Nod.png)

![cheaty](https://i.imgur.com/k1cHbQy.png)

<!--MDGEN_START-->
## Solutions

| Tag | Description | Multithreaded | Passes | Algorithm | Bits | Faithful |
|-----|-------------|---------------|--------|-----------|------|----------|
| SieveRT_64 | None given for this solution. | staticThreads | 39719 | base | 64 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | staticThreads | 25259 | base | 1 | true |
| SieveRT_32 | None given for this solution. | staticThreads | 25228 | base | 32 | true |
| SieveRT_8 | An 8-bit variant of SieveRT. | staticThreads | 21797 | base | 8 | true |
| SieveRT_64 | None given for this solution. | dynamicThreads | 21597 | base | 64 | true |
| SieveRT_16 | None given for this solution. | staticThreads | 21581 | base | 16 | true |
| SieveRT_32 | None given for this solution. | dynamicThreads | 14709 | base | 32 | true |
| SieveRT_16 | None given for this solution. | dynamicThreads | 13665 | base | 16 | true |
| SieveRT_8 | An 8-bit variant of SieveRT. | dynamicThreads | 13625 | base | 8 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | dynamicThreads | 12489 | base | 1 | true |
| SieveRT_64 | None given for this solution. | single | 11039 | base | 64 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | single | 7007 | base | 1 | true |
| SieveRT_32 | None given for this solution. | single | 6945 | base | 32 | true |
| SieveRT_8 | An 8-bit variant of SieveRT. | single | 6343 | base | 8 | true |
| SieveRT_16 | None given for this solution. | single | 5946 | base | 16 | true |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | staticThreads | 348082075 | other | 0 | false |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | single | 94957217 | other | 0 | false |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | dynamicThreads | 5376145 | other | 0 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | staticThreads | 44962 | lookup | 1 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | dynamicThreads | 29237 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | staticThreads | 28621 | base | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | dynamicThreads | 25169 | base | 1 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | single | 12996 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | single | 7780 | base | 1 | false |


## Output

```
Command: dub run -b release --compiler=ldc2 -- -m all
stderr:
    Passes: 7780, Time: 5 secs, 358 μs, and 9 hnsecs, Avg: 642 μs and 7 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 28621, Time: 5 secs, 649 μs, and 4 hnsecs, Avg: 174 μs and 7 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 25169, Time: 5 secs, 680 μs, and 8 hnsecs, Avg: 198 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 7007, Time: 5 secs, 345 μs, and 9 hnsecs, Avg: 713 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 25259, Time: 5 secs, 676 μs, and 3 hnsecs, Avg: 197 μs and 9 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 12489, Time: 5 secs, 161 μs, and 9 hnsecs, Avg: 400 μs and 3 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 94957217, Time: 5 secs and 1 hnsec, Avg: 0 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 348082075, Time: 5 secs, 7 μs, and 5 hnsecs, Avg: 0 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 5376145, Time: 5 secs, 10 μs, and 5 hnsecs, Avg: 9 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 12996, Time: 5 secs, 250 μs, and 3 hnsecs, Avg: 384 μs and 7 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 29237, Time: 5 secs, 412 μs, and 5 hnsecs, Avg: 171 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 44962, Time: 5 secs, 303 μs, and 2 hnsecs, Avg: 111 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 6343, Time: 5 secs, 221 μs, and 6 hnsecs, Avg: 788 μs and 3 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 13625, Time: 5 secs, 912 μs, and 8 hnsecs, Avg: 367 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 21797, Time: 5 secs, 855 μs, and 6 hnsecs, Avg: 229 μs and 4 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 5946, Time: 5 secs, 52 μs, and 7 hnsecs, Avg: 840 μs and 9 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 13665, Time: 5 secs, 9 ms, and 257 μs, Avg: 366 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 21581, Time: 5 secs, 10 ms, 880 μs, and 8 hnsecs, Avg: 232 μs and 1 hnsec, Limit: 1000000, Count: 78498, Valid: true
    Passes: 6945, Time: 5 secs, 36 μs, and 8 hnsecs, Avg: 719 μs and 9 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 14709, Time: 5 secs, 151 μs, and 3 hnsecs, Avg: 339 μs and 9 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 25228, Time: 5 secs and 337 μs, Avg: 198 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 11039, Time: 5 secs, 76 μs, and 6 hnsecs, Avg: 452 μs and 9 hnsecs, Limit: 1000000, Count: 41603, Valid: false
    Passes: 21597, Time: 5 secs, 297 μs, and 9 hnsecs, Avg: 231 μs and 5 hnsecs, Limit: 1000000, Count: 41603, Valid: false
    Passes: 39719, Time: 5 secs, 323 μs, and 9 hnsecs, Avg: 125 μs and 8 hnsecs, Limit: 1000000, Count: 41603, Valid: false

stdout:
    BradleyChatha-Single-SieveCT;7780;5.00036;1;algorithm=base,bits=1,faithful=no
    BradleyChatha-MultistaticThreads-SieveCT;28621;5.00065;4;algorithm=base,bits=1,faithful=no
    BradleyChatha-MultidynamicThreads-SieveCT;25169;5.00068;4;algorithm=base,bits=1,faithful=no
    BradleyChatha-Single-SieveRT;7007;5.00035;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT;25259;5.00068;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT;12489;5.00016;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-Single-SieveRTCT_Cheatiness;94957217;5;1;algorithm=other,bits=0,faithful=no
    BradleyChatha-MultistaticThreads-SieveRTCT_Cheatiness;348082075;5.00001;4;algorithm=other,bits=0,faithful=no
    BradleyChatha-MultidynamicThreads-SieveRTCT_Cheatiness;5376145;5.00001;4;algorithm=other,bits=0,faithful=no
    BradleyChatha-Single-SieveRT_LookupTable;12996;5.00025;1;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-MultidynamicThreads-SieveRT_LookupTable;29237;5.00041;4;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-MultistaticThreads-SieveRT_LookupTable;44962;5.0003;4;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-Single-SieveRT_8;6343;5.00022;1;algorithm=base,bits=8,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT_8;13625;5.00091;4;algorithm=base,bits=8,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT_8;21797;5.00086;4;algorithm=base,bits=8,faithful=yes
    BradleyChatha-Single-SieveRT_16;5946;5.00005;1;algorithm=base,bits=16,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT_16;13665;5.00926;4;algorithm=base,bits=16,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT_16;21581;5.01088;4;algorithm=base,bits=16,faithful=yes
    BradleyChatha-Single-SieveRT_32;6945;5.00004;1;algorithm=base,bits=32,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT_32;14709;5.00015;4;algorithm=base,bits=32,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT_32;25228;5.00034;4;algorithm=base,bits=32,faithful=yes
    BradleyChatha-Single-SieveRT_64;11039;5.00008;1;algorithm=base,bits=64,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT_64;21597;5.0003;4;algorithm=base,bits=64,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT_64;39719;5.00032;4;algorithm=base,bits=64,faithful=yes
```

