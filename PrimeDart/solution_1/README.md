# Dart solution by Eagerestwolf

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This implementation of the Prime Sieve uses the Dart Programming Language,
created by Google. The source file `bin/PrimeDart.dart` contains a lot of
comments to allow someone not familiar with Dart to understand what the code is
doing and why some things look a bit different to many other object oriented
programming languages (i.e. Java, C#, C++, etc). The reason such thorough
documentation is included is simple, Dave himself uses this algorithm when
learning a new language, so I think this is a good opportunity to teach others
a bit about Dart. So feel free to open that source file and take a read for
yourself!

## Run instructions

**NOTE**: Owners of an Apple Silicon Mac *must* use the Dart SDK method. The
Docker method will not work because Google does not publish arm64 images for
Dart, and QEMU does not like running x64 images on arm64. Not an issue with my
code, it's a known issue with Docker/QEMU/Dart's Docker images.

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
dart compile exe bin/PrimeDart.dart
```

Then run the binary with the following command:

```
bin/PrimeDart.exe
```

**NOTE**: The executable will always have a `.exe` extension, regardless of
platform, but Dart will generate a standard ELF binary on *nix systems.

## Output

### Machine Specifications

* **Model**: MSI GF63 Thin 10SCXR
* **CPU**: Intel Core i5-10300H
* **Memory**: 16GB DDR4
* **GPU**: Nvidia GeForce GTX 1650 with Max-Q Design
* **OS**: Windows 10 (Build 19042)

### Docker Results

```
Passes: 4548, Time: 5.000984, Avg: 0.0010996007036059806, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

eagerestwolf&mmcdon20;4548;5.000984;1;algorithm=base,faithful=yes,bits=8
```

### Dart SDK

```
Passes: 4150, Time: 5.000978, Avg: 0.001205054939759036, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

eagerestwolf&mmcdon20;4150;5.000978;1;algorithm=base,faithful=yes,bits=8
```

### Dart Compiled

```
Passes: 4610, Time: 5.000347, Avg: 0.0010846739696312364, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

eagerestwolf&mmcdon20;4610;5.000347;1;algorithm=base,faithful=yes,bits=8
```
