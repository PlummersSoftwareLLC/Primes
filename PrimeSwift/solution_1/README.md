# Swift solutions by j-f1, yellowcub, and fahlman

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This folder contains three single-threaded implementations: an array of 8-bit
Booleans, packed UInt8 bits, and striped UInt8 bits. The striped entry uses one bit
per odd candidate and marks composites with dense small-factor handlers and an
unrolled sparse loop. The Boolean and packed implementations are unchanged.

Credits:

1. j-f1 — original implementation and primary code arrangement.
2. yellowcub — performance improvements and the original one-bit implementations.
3. fahlman — dense marking and sparse-loop improvements to the striped entry,
   developed and reviewed with assistance from Claude and Codex.

## Striped implementation

Each pass allocates a fresh class-owned buffer, initializes it to zero, discovers
factors by checking odd candidates in ascending order, and marks their multiples
starting at p². A set bit means composite. Factor 3 uses a byte handler, odd
factors 5–63 use 64-bit handlers, and odd factors 65–111 use 128-bit handlers.
Larger factors use a fused loop with sixteen individual marks per iteration.

Every composite receives its own single-bit OR in Swift source. Each specialized
range includes every odd factor, and dispatch happens only after the runtime
primality test. There are no precomputed prime lists, composite-pattern tables,
wheel, presieving, or state retained between passes. The compiler may combine
individual source operations into wider stores. The `base,faithful=yes,bits=1`
classification describes those source operations.

Allocation, initialization, sieving, an opaque observation, and release are timed.
`BenchmarkObserver` is compiled as a separate SwiftPM target so the executable
optimizer cannot remove the completed sieve. The striped package explicitly
disables SwiftPM's default cross-module optimization for its observer and
executable targets. Enumeration, result checking, and printing occur outside timing.

## Run instructions

Use Swift 6.3.3. From this folder, build and run all three entries:

```sh
swift build -c release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_1bit_u8
swift build -c release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_1bitStriped_u8
swift build -c release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_8bitBool
./run.sh
```

Or build and run the Docker image:

```sh
docker build -t prime-swift .
docker run --rm prime-swift
```

The image builds with the official Swift 6.3.3 image and uses its slim runtime.
The default run benchmarks each entry for at least five seconds at 1,000,000.
The striped executable also retains `--upper-limit` / `-n`, `--time` / `-t`, and
Boolean-valued `--list-results` / `-l`:

```sh
./PrimeSwift_1bitStriped_u8/.build/release/PrimeSieveSwift -n 31 -t 0 -l true
```

The CLI lists primes strictly below the specified upper limit, matching the
existing interface. Its reusable `PrimeSieve` class includes its limit. These
conventions agree at the default even limit. Striped auxiliary output goes to
stderr; its standard benchmark record goes to stdout. A zero-duration diagnostic
request performs one complete pass; the benchmark default remains five seconds.

## Verification

The striped package includes one verifier against an independent Boolean
sieve, covering small limits, alignment, group and tail boundaries, random
limits, prime-square boundaries, one million and ten million. From
`PrimeSwift_1bitStriped_u8`:

```sh
mkdir -p .build
swift Tools/generate-dense.swift --check Sources/PrimeSieveSwift/PrimeSieve.swift
swiftc -O -sanitize=address Sources/PrimeSieveSwift/PrimeSieve.swift Tools/Verify.swift -o .build/verify-asan
.build/verify-asan
swiftc -O -whole-module-optimization Sources/PrimeSieveSwift/PrimeSieve.swift Tools/Verify.swift -o .build/verify
.build/verify
```

## Editing the generated sieve

Edit `Tools/PrimeSieve.swift.in` for handwritten sieve logic and
`Tools/generate-dense.swift` for the dense marking metadata. The generator inserts
the individual 64-bit and 128-bit source marks and renders the complete committed
`Sources/PrimeSieveSwift/PrimeSieve.swift`. Builds compile that output directly;
generation is not a build step or part of the runtime.

From `PrimeSwift_1bitStriped_u8`, regenerate and check the complete file:

```sh
swift Tools/generate-dense.swift --write Sources/PrimeSieveSwift/PrimeSieve.swift
swift Tools/generate-dense.swift --check Sources/PrimeSieveSwift/PrimeSieve.swift
```

`--write` replaces the entire output, including edits outside the generated
blocks; make those edits in the template instead. It requires an existing output
with both correctly ordered marker pairs, validates the template and dispatch
contract, then writes atomically. `--check` compares the complete output bytes.
The template defaults to `PrimeSieve.swift.in` beside the generator; append
`--template path/to/PrimeSieve.swift.in` to select another template explicitly.
With no arguments, the generator prints both marked switches to standard output.

fahlman's contributions in this folder are licensed under the BSD-3-Clause license, as CONTRIBUTING.md asks of every solution.

## Output

On an Apple M4 Pro (macOS 26.6.2, Swift 6.3.3), after the build commands above, `./run.sh` printed:

```
Passes: 13784, Time: 5.000250935554504, Avg: 0.000362757612852184, Limit: 1000000, Count: 78498, Valid: true

yellowcub_1bit_UInt8;13784;5.000250935554504;1;algorithm=base,faithful=yes,bits=1

yellowcub_fahlman_striped_UInt8;127639;5.000029667;1;algorithm=base,faithful=yes,bits=1

Passes: 16749, Time: 5.000002026557922, Avg: 0.00029852540608740356, Limit: 1000000, Count: 78498, Valid: true

j-f1_yellowcub_bool;16749;5.000002026557922;1;algorithm=base,faithful=yes,bits=8
```

The striped entry reports its diagnostic on standard error:

```
Passes: 127639, Time: 5.000029667, Avg: 3.9173212474243765e-05, Limit: 1000000, Count: 78498, Valid: true, Checksum: 27413282
```
