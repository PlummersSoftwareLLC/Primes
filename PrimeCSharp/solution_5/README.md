# C# solution by pbednarcik

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Single-threaded, 1-bit, faithful. I wanted to see how far C# can go past `PrimeCSharp/solution_4` without leaving the
base algorithm.

Most of the ideas are borrowed. The marking is @GordonBGood's "extreme-hybrid" ported to C#, and the run-time vector
masks follow `PrimeCPP/solution_5`. What I added: all three loops run one L1-sized segment at a time, and the vector
pattern is built by marking 64 multiples and copying the rest.

Both came from looking at where the time went, not from a plan:

- **Segments.** The bit array for 1,000,000 is 62.5 KB. To my knowledge most modern x86 CPUs have an L1 data cache of
  32 to 48 KB, so every factor walked through memory that did not fit. solution_4 already marks in blocks for its
  one loop. I tried the same for the whole sieve and it was 7 to 10% faster. A 32 KB segment fits either cache size.
- **Copying the pattern.** A profile showed the vector loop running about four times slower than its stores should
  allow. The JIT's assembly for the loop itself was fine: seven instructions per vector. A good part of the time
  went into building the pattern, 256 single-bit marks per factor, again in every segment. The pattern repeats after
  `factor` words, so 64 marks and two copies give the same result. That was 8.7% faster. It is simply less work, so
  it should help on any CPU.

One more thing turned up when I ported this to Go. The buffer that holds the patterns is on the stack and can start
at any multiple of 8 bytes, so some of the 32-byte pattern loads were split over two cache lines. Starting the
patterns on a cache line was 4% faster.

The figures are from one machine (AMD Ryzen 9 9950X). On a CPU with a larger L1 cache the segments should matter
less, and on arm64 the vectors are 128 bits wide, because .NET has nothing wider there.

## How it works

- One bit per odd number in a `byte[]`. A set bit means composite.
- The next factor is found by testing the bits in order, up to the square root.
- Marking starts at `factor * factor`. Which loop does it depends on the factor:
  - **up to 95:** the pattern of multiples repeats every `factor` 256-bit vectors, so it is built once and OR-ed in,
    one vector per store. Where `Vector256` is not accelerated (arm64) the same loop runs with `Vector128`, and
    with no vectors at all these factors use the word loop.
  - **up to 129:** the same with 64-bit words. Each word is read once, gets its bits, and is written once.
  - **above that:** the "extreme" loop, eight single-bit ORs per round with fixed masks and offsets.
- The buffer is done in 32 KB segments so the marking stays in L1. Each factor continues in the next segment where
  it stopped.
- The word and "extreme" loops are 96 generated methods. `tools/GenMarkers.cs` writes them during the build, so the
  generated file is not checked in.

No `unsafe` and no pointers. The "extreme" loop uses `Unsafe.Add` behind one range check per call. Little-endian
only, and I only ran it on x64.

It is labelled `base` like the other extreme-hybrid solutions: small factors set several multiples per store, and the
marking is done segment by segment. Each multiple is still marked by its own factor.

## Run instructions

### Docker

```
docker build -t primes-csharp-5 .
docker run --rm primes-csharp-5
```

### Native

Needs the .NET 10 SDK.

```
dotnet run --file tools/GenMarkers.cs -- Markers.Generated.cs
dotnet run -c Release
```

The program runs 1 second of untimed passes first, so the JIT is finished before the clock starts. `--warmup`,
`--segment`, `--vecmax` and `--stats` are there for experiments. A different segment size or vector cutoff shows up
in the label.

## Output

Ryzen 9 9950X, Windows 11, .NET 10.0.15, in Docker. The best three of 13 runs:

```
pbednarcik;132920;5.00001;1;algorithm=base,faithful=yes,bits=1
pbednarcik;132498;5.00002;1;algorithm=base,faithful=yes,bits=1
pbednarcik;132460;5.00002;1;algorithm=base,faithful=yes,bits=1
```

## Credits

- @GordonBGood for the extreme and extreme-hybrid marking, and @serg-gini for the D version of it.
- @davepl and Pol Marcet for the run-time vector masks in `PrimeCPP/solution_5`.
- @ItalyToast and the contributors of `PrimeCSharp/solution_4`. That was the one to beat the whole time.