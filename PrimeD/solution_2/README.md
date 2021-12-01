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

## Implementations

When an algorithm says it uses the 'base-unrolled' algorithm, it means it uses the base algorithm with a slightly unrolled loop.

### SieveCT

**Algorithm:** base-ex

**Bits:** 1

This sieve statically allocates its buffer because it knows the sieve size at compile time.

### SieveRT

**Algorithm:** base-ex

**Bits:** 1

This sieve dynamically allocates its buffer, so it can be faithful.

### SieveRTCT_Cheatiness

**Algorithm:** base-ex/none

**Bits:** 1 or 0, depending on your optics.

This sieve generates the entire sieve at compile time, and only stores the actual result, instead of the sieve itself (hence, either 1 or 0 bits depending on how you look at it).

This is achieveable because in D, most code can be run at compile time without any issue, so we simply run the `SieveRT` at compile time, and then we can easily store the result.

### SieveRT_LookupTable

**Algorithm:** lookup

**Bits:** 1

This sieve simply looks up each prime from a compile-time calculated sieve. idk why I still even have this here, it's a bit pointless.

### SieveRTBX - e.g. SieveRTBX!ubyte, SieveRTBX!ushort etc

**Algorithm:** base-ex

**Bits:** Varies by 'X'

This sieve uses the full bit width as a boolean, instead of using 1 bit per prime.

For example, if we're using a `ushort[]`, `ushort[0]` is for the first number, `ushort[1]` for the second, instead of using bitwise logic.

This exists as I was just curious about the performance.

### SieveCT_MegaUnroll

**Algorithm:** base-unrolled

**Bits:** 1

This sieve completely unrolls the entire loop.

This is done by using D's `static foreach` construct, which is a compile-time `foreach` that duplicates its body upon every iteration.

So the code:

```d
const q = cast(size_t)((cast(double)SieveSize).sqrt);

static foreach(factor; iota(3, q, 2)) // We can use `iota` because we can run functions at compile time.
{
    if(!this.getBit(factor))
    {
        static foreach(i; iota(factor * factor, SieveSize, factor * 2))
        {
            this.setBit(i);
        }
    }
}
```

Is all that is needed to unroll the entire thing.

**Now**, the issue is that this requires enourmous amounts of memory to compile (200GB+, I don't know the exact number), to the point I can't actually get it to compile for a sieve size of 1,000,000, and so this sieve will never be compiled by default.

You have the outer loop looping to around 1000 times, then you have the inner loop looping... *a lot* of times, 1000 extra times. So it's easy to see why the compiler chokes.

Due to this limitation, this sieve is never compiled, but still exists as an interesting "what if" implementation.

Thankfully, with a bit of swap memory, you *can* compile this sieve for a size of 100,000, so if you compile the project with the `-c unrolled` flag, you can
run the sieve against the other sieves at a prime size of 100,000 instead of 1,000,000.

If for some ungodly reason you want to try and compile it for the full 1,000,000, compile the project with the `-c good_luck` flag instead.

### SieveRTB1_X

**Algorithm:** base-ex

**Bits:** 1

Similar to `SieveRT`, except the storage type can be: `ubyte[]`, `ushort[]`, `uint[]`, or `ulong[]`.

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
| SieveRTB1_32 | None given for this solution. | staticThreads | 37151 | base | 1 | true |
| SieveRTB1_64 | None given for this solution. | staticThreads | 32429 | base | 1 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | staticThreads | 32294 | base | 1 | true |
| SieveRTB1_16 | None given for this solution. | staticThreads | 28174 | base | 1 | true |
| SieveRTBX | None given for this solution. | staticThreads | 26525 | base | 8 | true |
| SieveRTB1_16 | None given for this solution. | dynamicThreads | 22201 | base | 1 | true |
| SieveRTB1_64 | None given for this solution. | dynamicThreads | 22185 | base | 1 | true |
| SieveRTB1_32 | None given for this solution. | dynamicThreads | 21925 | base | 1 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | dynamicThreads | 18797 | base | 1 | true |
| SieveRTBX | None given for this solution. | staticThreads | 17205 | base | 64 | true |
| SieveRTBX | None given for this solution. | staticThreads | 14064 | base | 16 | true |
| SieveRTBX | None given for this solution. | staticThreads | 11304 | base | 32 | true |
| SieveRTBX | None given for this solution. | dynamicThreads | 10953 | base | 8 | true |
| SieveRTB1_32 | None given for this solution. | single | 10468 | base | 1 | true |
| SieveRTB1_64 | None given for this solution. | single | 9337 | base | 1 | true |
| SieveRT | Sieve that where everything is allocated and computed at runtime. | single | 9248 | base | 1 | true |
| SieveRTB1_16 | None given for this solution. | single | 9218 | base | 1 | true |
| SieveRTBX | None given for this solution. | dynamicThreads | 8601 | base | 64 | true |
| SieveRTBX | None given for this solution. | single | 7869 | base | 8 | true |
| SieveRTBX | None given for this solution. | dynamicThreads | 7421 | base | 16 | true |
| SieveRTBX | None given for this solution. | dynamicThreads | 7149 | base | 32 | true |
| SieveRTBX | None given for this solution. | single | 5907 | base | 32 | true |
| SieveRTBX | None given for this solution. | single | 5562 | base | 16 | true |
| SieveRTBX | None given for this solution. | single | 5498 | base | 64 | true |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | staticThreads | 327714454 | other | 1 | false |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | single | 92756813 | other | 1 | false |
| SieveRTCT_Cheatiness | Sieve that is fully generated at compile-time, which is kind of cheating. | dynamicThreads | 6157861 | other | 1 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | staticThreads | 46699 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | staticThreads | 35087 | base | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | dynamicThreads | 31069 | base | 1 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | dynamicThreads | 28593 | lookup | 1 | false |
| SieveRT_LookupTable | Sieve that uses a compile-time generated lookup table. | single | 12785 | lookup | 1 | false |
| SieveCT | Sieve where some storage is statically allocated, and some calcs are compile-time evaluated. | single | 9862 | base | 1 | false |


## Output

```
Command: dub run -b release --compiler=ldc2 -- -m all
stderr:
    Passes: 9862, Time: 5 secs, 331 μs, and 8 hnsecs, Avg: 507 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 35087, Time: 5 secs, 519 μs, and 5 hnsecs, Avg: 142 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 31069, Time: 5 secs and 462 μs, Avg: 160 μs and 9 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 9248, Time: 5 secs, 620 μs, and 8 hnsecs, Avg: 540 μs and 7 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 32294, Time: 5 secs, 210 μs, and 5 hnsecs, Avg: 154 μs and 8 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 18797, Time: 5 secs, 467 μs, and 8 hnsecs, Avg: 266 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 92756813, Time: 5 secs, Avg: 0 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 327714454, Time: 5 secs, 1 μs, and 6 hnsecs, Avg: 0 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 6157861, Time: 5 secs, 11 μs, and 4 hnsecs, Avg: 8 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 12785, Time: 5 secs, 321 μs, and 3 hnsecs, Avg: 391 μs and 1 hnsec, Limit: 1000000, Count: 78498, Valid: true
    Passes: 28593, Time: 5 secs, 138 μs, and 8 hnsecs, Avg: 174 μs and 8 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 46699, Time: 5 secs, 321 μs, and 5 hnsecs, Avg: 107 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 7869, Time: 5 secs, 444 μs, and 2 hnsecs, Avg: 635 μs and 4 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 10953, Time: 5 secs and 936 μs, Avg: 456 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 26525, Time: 5 secs, 629 μs, and 3 hnsecs, Avg: 188 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 9218, Time: 5 secs, 499 μs, and 4 hnsecs, Avg: 542 μs and 4 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 22201, Time: 5 secs, 297 μs, and 5 hnsecs, Avg: 225 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 28174, Time: 5 secs, 437 μs, and 2 hnsecs, Avg: 177 μs and 4 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 10468, Time: 5 secs, 68 μs, and 9 hnsecs, Avg: 477 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 21925, Time: 5 secs and 309 μs, Avg: 228 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 37151, Time: 5 secs, 352 μs, and 2 hnsecs, Avg: 134 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 9337, Time: 5 secs, 374 μs, and 1 hnsec, Avg: 535 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 22185, Time: 5 secs, 356 μs, and 6 hnsecs, Avg: 225 μs and 3 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 32429, Time: 5 secs, 1 ms, 575 μs, and 6 hnsecs, Avg: 154 μs and 2 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 5562, Time: 5 secs, 534 μs, and 8 hnsecs, Avg: 899 μs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 7421, Time: 5 secs, 4 ms, 104 μs, and 5 hnsecs, Avg: 674 μs and 3 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 14064, Time: 5 secs, 1 ms, 351 μs, and 5 hnsecs, Avg: 355 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 5907, Time: 5 secs and 238 μs, Avg: 846 μs and 4 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 7149, Time: 5 secs, 1 ms, 171 μs, and 1 hnsec, Avg: 699 μs and 5 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 11304, Time: 5 secs, 882 μs, and 3 hnsecs, Avg: 442 μs and 3 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 5498, Time: 5 secs, 411 μs, and 8 hnsecs, Avg: 909 μs and 4 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 8601, Time: 5 secs, 4 ms, 756 μs, and 5 hnsecs, Avg: 581 μs and 8 hnsecs, Limit: 1000000, Count: 78498, Valid: true
    Passes: 17205, Time: 5 secs, 776 μs, and 9 hnsecs, Avg: 290 μs and 6 hnsecs, Limit: 1000000, Count: 78498, Valid: true

stdout:
    BradleyChatha-Single-SieveCT;9862;5.00033;1;algorithm=base,bits=1,faithful=no
    BradleyChatha-MultistaticThreads-SieveCT;35087;5.00052;4;algorithm=base,bits=1,faithful=no
    BradleyChatha-MultidynamicThreads-SieveCT;31069;5.00046;4;algorithm=base,bits=1,faithful=no
    BradleyChatha-Single-SieveRT;9248;5.00062;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRT;32294;5.00021;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRT;18797;5.00047;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-Single-SieveRTCT_Cheatiness;92756813;5;1;algorithm=other,bits=1,faithful=no
    BradleyChatha-MultistaticThreads-SieveRTCT_Cheatiness;327714454;5;4;algorithm=other,bits=1,faithful=no
    BradleyChatha-MultidynamicThreads-SieveRTCT_Cheatiness;6157861;5.00001;4;algorithm=other,bits=1,faithful=no
    BradleyChatha-Single-SieveRT_LookupTable;12785;5.00032;1;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-MultidynamicThreads-SieveRT_LookupTable;28593;5.00014;4;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-MultistaticThreads-SieveRT_LookupTable;46699;5.00032;4;algorithm=lookup,bits=1,faithful=no
    BradleyChatha-Single-SieveRTBX;7869;5.00044;1;algorithm=base,bits=8,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRTBX;10953;5.00094;4;algorithm=base,bits=8,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRTBX;26525;5.00063;4;algorithm=base,bits=8,faithful=yes
    BradleyChatha-Single-SieveRTB1_16;9218;5.0005;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRTB1_16;22201;5.0003;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRTB1_16;28174;5.00044;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-Single-SieveRTB1_32;10468;5.00007;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRTB1_32;21925;5.00031;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRTB1_32;37151;5.00035;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-Single-SieveRTB1_64;9337;5.00037;1;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRTB1_64;22185;5.00036;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRTB1_64;32429;5.00158;4;algorithm=base,bits=1,faithful=yes
    BradleyChatha-Single-SieveRTBX;5562;5.00053;1;algorithm=base,bits=16,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRTBX;7421;5.0041;4;algorithm=base,bits=16,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRTBX;14064;5.00135;4;algorithm=base,bits=16,faithful=yes
    BradleyChatha-Single-SieveRTBX;5907;5.00024;1;algorithm=base,bits=32,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRTBX;7149;5.00117;4;algorithm=base,bits=32,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRTBX;11304;5.00088;4;algorithm=base,bits=32,faithful=yes
    BradleyChatha-Single-SieveRTBX;5498;5.00041;1;algorithm=base,bits=64,faithful=yes
    BradleyChatha-MultidynamicThreads-SieveRTBX;8601;5.00476;4;algorithm=base,bits=64,faithful=yes
    BradleyChatha-MultistaticThreads-SieveRTBX;17205;5.00078;4;algorithm=base,bits=64,faithful=yes
```

