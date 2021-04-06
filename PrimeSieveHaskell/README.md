# PrimeSieveHaskell

In order to run it, you need the [`Stack`](https://docs.haskellstack.org/en/stable/README/) tool
for Haskell. Note that some distribution-packaged versions of stack are outdated
(e.g. on Debian derived distributions), so following the installation instructions from the link
above is advised.

Additionally, LLVM 9 is needed in order to make use of the LLVM backend of GHC.

In total, there are four different variants: a `Vector Bool`-based implementation and a
`BitSet`-based one, and both with and without bounds checking for array accesses. The bit set
omitting the bounds checks is closest to the C++ `vector<bool>`.

The variants can be run as follows:

```bash
stack run VectorBool
stack run VectorBoolUnchecked
stack run BitSet
stack run BitSetUnchecked
```

Stack will automatically pull in the GHC compiler and any dependencies needed for compiling this
program.

## Benchmark results

RUn on an AMD Ryzen 7 3800X using the Haskell LLVM backend.
The runs have been extended to 60s to average out fluctuations.

```
CPP                 Passes: 128166, Time: 60.000000, Avg: 0.000468, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
VectorBool          Passes: 159233, Time: 60.000048, Avg: 0.000377, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
VectorBoolUnchecked Passes: 180499, Time: 60.000091, Avg: 0.000332, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
BitSet              Passes: 124539, Time: 60.000117, Avg: 0.000482, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
BitSetUnchecked     Passes: 138751, Time: 60.000165, Avg: 0.000432, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
```

The bit set implementation is quite close to the performance of the C++ implementation,
although omitting bound checks seems to give it a slight edge.

Interestingly, the vector based implementation (where one bool is one `Word8`) is a lot faster.
Probably, because on this particular CPU, the additional memory accesses of the vector
implementation are still faster than the additional bit-twiddling needed for the bit sets.
