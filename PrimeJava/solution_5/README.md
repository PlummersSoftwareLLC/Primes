# Java solution by pbednarcik

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Single-threaded, 1-bit, faithful. I wanted to see how far Java can go past `PrimeJava/solution_4` without leaving the
base algorithm. It is the same sieve as my `PrimeCSharp/solution_5` and `PrimeGo/solution_5`, written again in Java.

Most of the ideas are borrowed. The marking is @GordonBGood's "extreme-hybrid", and the run-time vector masks follow
`PrimeCPP/solution_5`. What I added: all loops run one L1-sized segment at a time, and the vector pattern is built by
marking 64 multiples and copying the rest.

- **Segments.** The bit array for 1,000,000 is 62.5 KB. To my knowledge most modern x86 CPUs have an L1 data cache of
  32 to 48 KB, so every factor walked through memory that did not fit. A 32 KB segment fits either cache size.
- **Copying the pattern.** The vector pattern of a factor holds 256 multiples. It repeats after `factor` 64-bit
  words, so 64 marks and two copies give the same result with less work.

It runs twice and prints two lines:

- `pbednarcik-java` uses plain arrays only.
- `pbednarcik-java-unsafe` is the same sieve with `sun.misc.Unsafe` in one loop.

Some things are specific to Java. I found them by looking at where the time went:

- **Vectors.** The small factors use the Vector API. It is still an incubator module, so `javac` and `java` need
  `--add-modules jdk.incubator.vector`.
- **Bounds checks.** The "extreme" loop writes eight bytes per round, and Java checks every one of them. With
  `sun.misc.Unsafe` the whole sieve is about a quarter faster. The JDK has deprecated that access for removal, which
  is why it is a variant of its own and not the only one.
- **No generated code.** The eight masks of the "extreme" loop depend only on the factor modulo 8 and on the bit
  where a run starts, which makes 32 cases. My C# and Go versions generate one loop per case with the masks as
  constants. In Java one loop with the masks in variables was about 10% faster than the generated ones, so there is
  no generator.
- **Vector cutoff.** With the bounds checks the "extreme" loop costs more, so more factors are worth a vector
  pattern: up to 191 with plain arrays, up to 95 with `Unsafe`.
- **Garbage collector.** The only garbage is one sieve per pass. The serial collector with a small young generation
  was 9% faster than the default.

The figures are from one machine (AMD Ryzen 9 9950X). I did not run it on arm64.

## How it works

- One bit per odd number in a `byte[]`. A set bit means composite.
- The next factor is found by testing the bits in order, up to the square root.
- Marking starts at `factor * factor`. Which loop does it depends on the factor:
  - **small factors:** the pattern of multiples repeats every `factor` 256-bit vectors, so it is built once per pass
    and OR-ed in, one vector per store.
  - **above that:** the "extreme" loop, eight single-bit ORs per round at fixed offsets.
- The buffer is done in 32 KB segments so the marking stays in L1. Each factor continues in the next segment where
  it stopped.

It is labelled `base` like the other extreme-hybrid solutions: small factors set several multiples per store, and the
marking is done segment by segment. Each multiple is still marked by its own factor.

## Run instructions

### Docker

```
docker build -t primes-java-5 .
docker run --rm primes-java-5
```

### Native

I used JDK 25.

```
javac --add-modules jdk.incubator.vector -Xlint:-removal *.java
java --add-modules jdk.incubator.vector -XX:+UseSerialGC -Xmn32m Main
java --add-modules jdk.incubator.vector -XX:+UseSerialGC -Xmn32m --sun-misc-unsafe-memory-access=allow Main --unsafe
```

The program runs 2 seconds of untimed passes first, so the JIT is finished before the clock starts. `--segment`,
`--vecmax` and `--warmup` are there for experiments. A different segment size or vector cutoff shows up in the label.
`--test` compares the result with a plain sieve for every size up to 3,000 and 100 random sizes up to 3,000,000.

## Output

Ryzen 9 9950X, Windows 11, JDK 25, in Docker, with other work running on the machine:

```
pbednarcik-java;86891;5.000104;1;algorithm=base,faithful=yes,bits=1
pbednarcik-java-unsafe;115848;5.000103;1;algorithm=base,faithful=yes,bits=1
```

## Credits

- @GordonBGood for the extreme and extreme-hybrid marking, and @serg-gini for the D version of it.
- @davepl and Pol Marcet for the run-time vector masks in `PrimeCPP/solution_5`.
- @chrvanorle for `PrimeJava/solution_4`. That was the Java solution to beat.
