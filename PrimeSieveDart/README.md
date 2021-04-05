# PrimeSieve Dart

This folder contains a Dart implementation of David Plummer's PrimeSieve. This
code is essentially a carbon copy of the original C++ algorithm, with some Dart
quirks.

## Performance

<details>
  <summary>2020 MacBook Pro (Apple M1)</summary>
  
  ```txt
  Passes: 1335, Time: 5.002707, Avg: 0.0037473460674157305, Limit: 1000000, Count1: 1, Count2: 78498, Valid: true
  ```
  
</details>

## How to use it

Firstly, install Dart from https://dart.dev. Then, clone the repository and
open a terminal or command line in the folder where you cloned the code.
For example:

```bash
$ git clone https://github.com/davepl/Primes.git
$ cd Primes/PrimeSieveDart
```

Then, you have 2 options. For a debug configuration, simply type `dart run`.
Alternatively for maximum performance, run the following:

```bash
$ dart compile exe bin/PrimeSieveDart.dart
$ bin/PrimeSieveDart.exe
```

It should be noted that even though it says exe, on linux and macOS Dart
outputs standard ELF binaries, so it still works.
