# Dart solution by Eagerestwolf and mmcdon20

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

* **Model**: MSI GF63 Thin 10SCXR
* **CPU**: Intel Core i5-10300H
* **Memory**: 16GB DDR4
* **GPU**: Nvidia GeForce GTX 1650 with Max-Q Design
* **OS**: Windows 10 (Build 19042)

### Docker Results (runs all implementations)

```
eagerestwolf&mmcdon20_8bit;4661;5.000105;1;algorithm=base,faithful=yes,bits=8
eagerestwolf&mmcdon20_8bit_par;10710;5.001459;12;algorithm=base,faithful=yes,bits=8
eagerestwolf&mmcdon20_1bit;4234;5.000628;1;algorithm=base,faithful=yes,bits=1
eagerestwolf&mmcdon20_1bit_par;24644;5.001196;12;algorithm=base,faithful=yes,bits=1
```

### Dart SDK (running prime_8_bit.dart)

```
Passes: 5213, Time: 5.000862, Avg: 0.0009593059658545943, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

eagerestwolf&mmcdon20_8bit;5213;5.000862;1;algorithm=base,faithful=yes,bits=8
```

### Dart Compiled (running prime_8_bit.exe)

```
Passes: 5588, Time: 5.000777, Avg: 0.0008949135647816751, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

eagerestwolf&mmcdon20_8bit;5588;5.000777;1;algorithm=base,faithful=yes,bits=8
```
