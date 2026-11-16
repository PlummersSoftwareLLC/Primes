# Go solution by pbednarcik

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Single-threaded, 1-bit, faithful. This follows my `PrimeCSharp/solution_5`: the same sieve, written again in Go. I
wanted to see how close Go gets to C# now that it has vector types.

Most of the ideas are borrowed. The marking is @GordonBGood's "extreme-hybrid", and the run-time vector masks follow
`PrimeCPP/solution_5`. What I added there is the same here: all loops run one L1-sized segment at a time, and the
vector pattern is built by marking 64 multiples and copying the rest.

The port was quick. Getting it to the speed of the C# one was not, and again it came from looking at where the time
went:

- **Vectors.** The small factors use `simd/archsimd`. It is new and still experimental, so the build needs
  `GOEXPERIMENT=simd`. Without it the same source builds and uses the word loops, at about 65% of the speed.
- **Zeroing.** Go clears every buffer it hands out, also the one on the stack that holds the vector patterns. With a
  buffer four times larger than needed the whole sieve was 18% slower. Now it is only as large as the patterns of
  the primes up to 127.
- **Alignment.** That buffer can start at any multiple of 8 bytes, so many of the 32-byte pattern loads were split
  over two cache lines. Starting the patterns on a cache line was 6% faster.
- **Garbage collector.** The only garbage is one sieve per pass. With the collector's target at 400% instead of
  100% it runs less often, which was 8% faster. Switching it off was slower.

I also tried 512-bit vectors, smaller patterns and other segment sizes. All of them were slower.

The figures are from one machine (AMD Ryzen 9 9950X). On arm64 it runs without vectors, and I did not test it there.

## How it works

- One bit per odd number in a `[]uint64`. A set bit means composite.
- The next factor is found by testing the bits in order, up to the square root.
- Marking starts at `factor * factor`. Which loop does it depends on the factor:
  - **up to 127:** the pattern of multiples repeats every `factor` 256-bit vectors, so it is built once per pass
    and OR-ed in, one vector per store. This needs amd64 with AVX2.
  - **up to 129, where there are no vectors:** the same with 64-bit words. Each word is read once, gets its bits,
    and is written once.
  - **above that:** the "extreme" loop, eight single-bit ORs per round with fixed masks and offsets.
- The buffer is done in 32 KB segments so the marking stays in L1. Each factor continues in the next segment where
  it stopped.
- The word and "extreme" loops are 96 generated functions. `gen/main.go` writes them with `go generate`, so the
  generated file is not checked in.

No assembly, and no build flags other than the experiment. The "extreme" loop and the vector loop use `unsafe.Add`
behind one range check per call. Little-endian only.

It is labelled `base` like the other extreme-hybrid solutions: small factors set several multiples per store, and the
marking is done segment by segment. Each multiple is still marked by its own factor.

## Run instructions

### Docker

```
docker build -t primes-go-5 .
docker run --rm primes-go-5
```

### Native

I used Go 1.27. In PowerShell:

```
$env:GOEXPERIMENT = 'simd'
go generate
go run .
```

`-segment`, `-vecmax` and `-gc` are there for experiments. A setting other than the default shows up in the label.
`go test` compares the result with a plain sieve for every size up to 3,000 and 100 random sizes up to 3,000,000.

## Output

Ryzen 9 9950X, Windows 11, Go 1.27, in Docker. Three runs in a row:

```
pbednarcik-go;129920;5.000036;1;algorithm=base,faithful=yes,bits=1
pbednarcik-go;130383;5.000031;1;algorithm=base,faithful=yes,bits=1
pbednarcik-go;130806;5.000017;1;algorithm=base,faithful=yes,bits=1
```

## Credits

- @GordonBGood for the extreme and extreme-hybrid marking, and @serg-gini for the D version of it.
- @davepl and Pol Marcet for the run-time vector masks in `PrimeCPP/solution_5`.
- @ssovest for `PrimeGo/solution_2`. That was the Go solution to beat.
