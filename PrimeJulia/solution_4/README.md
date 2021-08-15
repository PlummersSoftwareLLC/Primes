# Julia solution by GordonBGood
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)

This is a "low-level" style implementation in
Julia to get as much as speed as possible out of the language. It is
**not** designed to be entirely idiomatic Julia code, but it does show the kind of performance that can be extracted from the language.

## Description

Instead of using Julia's native `BitVector` type, this solution
manually implements a bit array using a `Vector{UInt8}`; This algorithm needs to use a byte sieving buffer in order to keep the total number of sub loops no higher than eight, and is still fast because no "bit-twiddling" operation using shifts and masking are done in the main culling/marking loops.

This implementation only stores bits for the odd numbers above 3 in an "inverted" bit array, i.e., bits are set when the number is *not prime* and bits are unset when the number is *prime*. This simplifies the set_bit operation slightly (`arr[i] |= mask vs. arr[i] &= ~mask`).

The main secret to why it is so fast is the algorithm, which uses up to eight separate masking loops with one for each bit "stripe" in the byte sieve buffer so as to make the loops as simple as possible; as well, it does some manual loop unrolling (by four) and pages through the sieve buffer by CPU cache-sized pages to minimize (but not eliminate) the "cache thrashing" that would otherwise arise from running each of the eight loops across the entire sieve buffer.  For most common CPU's that have a 32 Kilobyte L1 cache, this means that almost all of the bytes in the cache need to be "thrashed" when culling across the entire almost 64 Kilobyte sieve buffer required to sieve to a million, at least for smaller base prime values.

## Run instructions

### Running locally

First, make sure that you have installed
[Julia 1.5 or newer](https://julialang.org/downloads/) and have
verified that your installation works.

To build and run the solution locally, run the following command:
```
julia primes_unpeeled.jl
```

### Running in Docker

If you want to run this solution in a Docker container, follow the steps below.

1. Build or update the Docker image. You can skip this step if you have
   already built the `primejulia-4` image.

```
docker build --pull -t primejulia-4 .
```

2. Run the `primejulia-4` image as a container.
```
docker run -it --rm primejulia-4
```
3. If you want to remove the built Docker image, run:
```
docker image rm primejulia-4
```

This solution's [Dockerfile](Dockerfile) uses the `julia:1.6-buster` image to provide maximum support for different architectures, most importantly ARM64 and x86_64. This also helps avoid any possible issues caused by Julia's [Tier 3 support for musl](https://julialang.org/downloads/#currently_supported_platforms), including Alpine Linux.


## Output

This is the output from running the solution on a machine with an Intel Core SkyLake i5-6500 CPU at 3.6 GHz (single threaded turbo):
```
GordonBGood_unpeeled;15137;5.000292062759399;1;algorithm=base,faithful=yes,bits=1
```

When running in Docker on the same machine, the output is as follows:

```
```
