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
| SieveRT_8 | An 8-bit variant of SieveRT. | true | 14713 | base | 8 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | true | 13313 | base | 1 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | false | 6956 | base | 1 | true |
| SieveRT_8 | An 8-bit variant of SieveRT. | false | 6534 | base | 8 | true |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | false | 92990407 | pregenerated | 0 | false |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | true | 4844121 | pregenerated | 0 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | true | 30861 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | true | 22689 | base | 1 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | false | 12998 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | false | 7547 | base | 1 | false |


## Output

```
Command: dub run -b release --compiler=ldc2
stderr:
    Passes: 7547, Time: 5 secs, 35 μs, and 1 hnsec, Avg: 662 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 22689, Time: 5 secs, 227 μs, and 1 hnsec, Avg: 220 μs and 3 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 6956, Time: 5 secs, 464 μs, and 9 hnsecs, Avg: 718 μs and 8 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 13313, Time: 5 secs, 1 ms, 124 μs, and 4 hnsecs, Avg: 375 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 92990407, Time: 5 secs, Avg: 0 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 4844121, Time: 5 secs, 6 μs, and 9 hnsecs, Avg: 1 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 12998, Time: 5 secs, 268 μs, and 4 hnsecs, Avg: 384 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 30861, Time: 5 secs, 182 μs, and 9 hnsecs, Avg: 162 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 6534, Time: 5 secs, 441 μs, and 8 hnsecs, Avg: 765 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 14713, Time: 5 secs, 7 μs, and 4 hnsecs, Avg: 339 μs and 8 hnsecs, Limit: 1000000, Count: 78498, Valid: true

stdout:
    BradleyChatha-Single-SieveCT;7547;5.00004;1;algorithm=base,bits=1,faithful=no
    BradleyChatha-Multi-SieveCT;22689;5.00023;4;algorithm=base,bits=1,faithful=no
    BradleyChatha-Single-SieveRT;6956;5.00046;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-Multi-SieveRT;13313;5.00112;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-Single-SieveRTCT_Cheatiness;92990407;5;1;algorithm=pregenerated,bits=0,faithful=no
    BradleyChatha-Multi-SieveRTCT_Cheatiness;4844121;5.00001;4;algorithm=pregenerated,bits=0,faithful=no
    BradleyChatha-Single-SieveRT_LookupTable;12998;5.00027;1;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-Multi-SieveRT_LookupTable;30861;5.00018;4;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-Single-SieveRT_8;6534;5.00044;1;algorithm=base,bits=8,faithful=yes
    BradleyChatha-Multi-SieveRT_8;14713;5.00001;4;algorithm=base,bits=8,faithful=yes
```

