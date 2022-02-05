# Dart ffi solution by mmcdon20

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is a faithful implementation where the core functionality written in c, and then loaded into dart using ffi.

This includes both a single-threaded and parallel implementation. 

## Run instructions

To run the solution using Docker, run the following command:

```
docker build -t primes-dart-ffi .
docker run --rm -it primes-dart-ffi
```

To run the solution locally, you need to have dart, gcc, and llvm installed, and then use the following commands:

```
dart bin/builder.dart
dart bin/runner.dart
```

### Output

```
mmcdon20_dart+c_1_bit;8732;5.000222;1;algorithm=base,faithful=yes,bits=1
mmcdon20_dart+c_1_bit_par;58560;5.000893;12;algorithm=base,faithful=yes,bits=1
```
