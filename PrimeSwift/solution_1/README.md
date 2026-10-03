# Swift solutions by j-f1, yellowcub, and fahlman

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This is a collection of three sieve implementations in Swift:

1. **8-bit Bool** (`PrimeSwift_8bitBool`) stores one Boolean per odd candidate in a
   buffer. Sieving clears each composite flag from the factor's square.
2. **Packed UInt8** (`PrimeSwift_1bit_u8`) stores eight odd candidates per byte.
   Sieving clears individual bits using a table of the eight single-bit masks.
3. **Striped UInt8** (`PrimeSwift_1bitStriped_u8`) stores eight odd candidates per
   byte, using generated handlers for small factors and sixteen marks per loop
   iteration for larger factors.

Credits:

1. j-f1 — original implementation and code arrangement.
2. yellowcub — performance improvements and one-bit implementations.
3. fahlman — dense and sparse marking improvements, with assistance from Claude
   and Codex.

Ideas for striped marking come from mike-barber's [Rust](../../PrimeRust/solution_1),
GordonBGood's [Nim](../../PrimeNim/solution_3), [Julia](../../PrimeJulia/solution_4)
and [Chapel](../../PrimeChapel/solution_1), and ManDeJan, ityonemo and SpexGuy's
[Zig](../../PrimeZig/solution_3) entries.
fahlman's contributions are licensed under BSD-3-Clause.

## Run instructions

With Swift 6.3.3, build and run from this folder:

```sh
swift build -c release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_1bit_u8
swift build -c release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_1bitStriped_u8
swift build -c release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_8bitBool
./run.sh
```

`run.sh` runs the implementations sequentially. Each run defaults to at least five
seconds with a limit of 1,000,000. Use an executable's `--help` for its options.

Or use Docker:

```sh
docker build -t prime-swift .
docker run --rm prime-swift
```

The x86-64 Docker image targets Haswell and requires a compatible CPU. The arm64
build uses the default target.

Striped [generation](PrimeSwift_1bitStriped_u8/Tools/generate-dense.swift) and
[verification](PrimeSwift_1bitStriped_u8/Tools/Verify.swift) commands are documented
in their source files; edit the [template](PrimeSwift_1bitStriped_u8/Tools/PrimeSieve.swift.in)
for sieve changes.

## Output

Example benchmark records from `./run.sh` on an Apple M4 Pro
(macOS 26.6.2, Swift 6.3.3):

```text
yellowcub_1bit_UInt8;13784;5.000250935554504;1;algorithm=base,faithful=yes,bits=1
yellowcub_fahlman_striped_UInt8;127639;5.000029667;1;algorithm=base,faithful=yes,bits=1
j-f1_yellowcub_bool;16749;5.000002026557922;1;algorithm=base,faithful=yes,bits=8
```
