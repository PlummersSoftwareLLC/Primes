# PrimeSieveHaskell

In order to run it, you need the [`Stack`](https://docs.haskellstack.org/en/stable/README/) tool
for Haskell. Note that some distribution-packaged versions of stack are outdated
(e.g. on Debian derived distributions), so following the installation instructions from the link
above is advised.

Additionally, LLVM 9 is needed in order to make use of the LLVM backend of GHC.

Then, it is simply a matter of running 

```bash
stack run
```

Stack will automatically pull in the GHC compiler and any dependencies needed for compiling this
program.

## Benchmark results

On an AMD Ryzen 7 3800X, the Haskell version comes in slightly ahead of the PrimeCPP version,
though only when using the LLVM backend.
The native backend seems to be slightly slower for this use case.

### Haskell (LLVM)

```
Passes: 11050, Time: 5.000360, Avg: 0.000453, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 11600, Time: 5.000207, Avg: 0.000431, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 10950, Time: 5.000238, Avg: 0.000457, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 11283, Time: 5.000261, Avg: 0.000443, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 11393, Time: 5.000352, Avg: 0.000439, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 11261, Time: 5.000014, Avg: 0.000444, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 11255, Time: 5.000274, Avg: 0.000444, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 11539, Time: 5.000209, Avg: 0.000433, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 11569, Time: 5.000154, Avg: 0.000432, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 11524, Time: 5.000242, Avg: 0.000434, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
```

### Haskell (native)

```
Passes: 9193, Time: 5.000282, Avg: 0.000544, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 8789, Time: 5.000020, Avg: 0.000569, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 8320, Time: 5.000327, Avg: 0.000601, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 8358, Time: 5.000242, Avg: 0.000598, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 8440, Time: 5.000016, Avg: 0.000592, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 10529, Time: 5.000008, Avg: 0.000475, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 8972, Time: 5.000204, Avg: 0.000557, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 8940, Time: 5.000434, Avg: 0.000559, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 8908, Time: 5.000171, Avg: 0.000561, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
Passes: 8523, Time: 5.000391, Avg: 0.000587, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
```

### PrimeCPP

```
Passes: 10549, Time: 5.000078, Avg: 0.000474, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 10229, Time: 5.000241, Avg: 0.000489, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 10396, Time: 5.000207, Avg: 0.000481, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 10563, Time: 5.000259, Avg: 0.000473, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 10546, Time: 5.000413, Avg: 0.000474, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 10569, Time: 5.000382, Avg: 0.000473, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 10379, Time: 5.000249, Avg: 0.000482, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 10574, Time: 5.000063, Avg: 0.000473, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 10401, Time: 5.000271, Avg: 0.000481, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
Passes: 10554, Time: 5.000258, Avg: 0.000474, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
```
