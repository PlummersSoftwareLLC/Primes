# Dart solution by Tarish

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This implementation of the Prime Sieve uses the Dart Programming Language,
created by Google. The source file `bin/prime_8_bit.dart` contains a lot of
comments to allow someone not familiar with Dart to understand what the code is
doing and why some things look a bit different to many other object oriented
programming languages (i.e. Java, C#, C++, etc). The reason such thorough
documentation is included is simple, Dave himself uses this algorithm when
learning a new language, so I think this is a good opportunity to teach others
a bit about Dart. So feel free to open that source file and take a read for
yourself!

## New Optimized Implementations

This repository includes several optimized variants:

- **`prime_8_bit.dart`**: 8-bit implementation with optimized memory access patterns
- **`prime_1_bit.dart`**: 1-bit implementation with efficient bit manipulation
- **`prime_8_bit_par.dart`**: Parallel 8-bit implementation using Dart's Isolate API
- **`prime_1_bit_par.dart`**: Parallel 1-bit implementation using Dart's Isolate API

All implementations use the standard Sieve of Eratosthenes algorithm with various performance optimizations including:
- Loop unrolling for better CPU pipeline utilization
- Optimized memory access patterns
- Efficient bit manipulation for 1-bit variants
- Reduced function call overhead

## Run instructions

To run this solution, you must either have [Docker](https://www.docker.com) or
the [Dart SDK](https://dart.dev) installed.

To run the solution using Docker, run the following command:

```
docker build -t primes-dart .
docker run --rm -it primes-dart
```

To run the solution using the DartSDK, run the following command:

```
dart run
```

### Building and running

Dart supports compiling a native binary for your operating system and 
architecture. This is only supported using the Dart SDK, but can lead to 
increased speed. To compile the binary, run the following command:

```
dart compile exe bin/prime_8_bit.dart
```

Then run the binary with the following command:

```
bin/prime_8_bit.exe
```

**NOTE**: The executable will always have a `.exe` extension, regardless of
platform, but Dart will generate a standard ELF binary on *nix systems.

## Output

### Machine Specifications

* **Model**: HP Omen 16-n0123AX
* **CPU**: AMD Ryzen 7 6800H
* **Memory**: 40GB DDR5
* **GPU**: Nvidia GeForce RTX 3070 Ti Laptop GPU
* **OS**: Ubuntu 25.04

### Docker Results (runs all implementations)

```
tarish_8bit;9200;5.000127;1;algorithm=base,faithful=yes,bits=8
tarish_8bit_par;51705;5.001276;16;algorithm=base,faithful=yes,bits=8
tarish_1bit;9332;5.000479;1;algorithm=base,faithful=yes,bits=1
tarish_1bit_par;56601;5.001923;16;algorithm=base,faithful=yes,bits=1
```

### Dart SDK (running prime_8_bit.dart)

```
Passes: 7949, Time: 5.000058, Avg: 0.000629017234872311, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

tarish_8bit;7949;5.000058;1;algorithm=base,faithful=yes,bits=8
```

### Dart Compiled (running prime_8_bit.exe)

```
Passes: 9283, Time: 5.000049, Avg: 0.0005386242593989011, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

tarish_8bit;9283;5.000049;1;algorithm=base,faithful=yes,bits=8
```

