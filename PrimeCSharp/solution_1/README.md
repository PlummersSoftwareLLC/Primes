# Prime Sieve Algorithms

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## C# solution by @Kinematics

This is a collection of prime sieve algorithms implemented in C#, using .NET 6.

## Building

Visual Studio: You can build this using Visual Studio 2022 (17.0) or later.

Commandline: `dotnet build -c release` from within the solution directory.  The output will be placed in `.\bin\x64\Release\net6.0\`.

Docker: From the commandline, in the `PrimeCSharp\solution_1` directory, run `docker build -t PrimeCSharp_1 .`

## Run instructions

If you run from Visual Studio, you can put commandline arguments in the debug tab of the project properties. You can run it without the debugger attaching to the process by hitting Ctrl-F5.

To run the Docker container, use the command `docker run -rm PrimeCSharp_1 [options]`, using the options provided below.

## Options

Options you can set from the commandline:

* -s, --size: The size of the sieve to run. Must be at least 10. Default: 1000000
* -t, --time: How many seconds to run the process. Default: 5 seconds
* -v, --verbose: Display the primes that were found once the run is complete. Defaults to off.
* -m, --multi: Run multithreaded, with each thread running its own instance of the sieve. Defaults to off.
* --threads: How many threads to use when running multithreaded. Defaults to the number of processors on the system.
* --pthreads: How many threads to use when running parallel implementations. No effect on non-parallel sieves.


### V1 options

To use legacy (V1) sieves, the first commandline argument must be `v1`.

V1 sieves that can be run:

* --original — The original code, with only bugfixes applied.
* --standard — Cleaned up variant of the original.
* --bool — Uses a bool[] instead of a BitArray.
* --ibool — Inverts the boolean logic used, to avoid double initialization.
* --dbool — Inlines references, using the ibool as the base.
* --raw — Allocates byte[] data directly through the GC.
* --raw32 — Variant that uses uint[] instead of byte[].
* --rawdirect — Inlines some references.
* --pool — Uses an array pool.
* --ref — A ref struct that uses an array pool.
* --pool6 — Uses an array pool, and calculations using mod 6 filters.
* --raw6 — The rawbits version with mod6 filters.
* --pool6p — The array pool using mod 6 filters, parallelized.
* --rawp — The rawbits version, parallelized.
* --pool30 — Uses an array pool, and calculations using mod 30 filters.

Only one of the above can be chosen at a time.  Alternatively, you can use `--all` to run all sieves.


### V2 options

V2 sieves that can be run:

* --bit2 — Uses a bitarray for storage, and the 1 of 2 algorithm.
* --bit2while — Uses a bitarray for storage, and the 1 of 2 algorithm. Uses a while loop instead of a for loop.
* --bit6 — Uses a bitarray for storage, and the 2 of 6 algorithm.
* --bit30 — Uses a bitarray for storage, and the 8 of 30 algorithm.
* --bool2 — Uses a bool array for storage, and the 1 of 2 algorithm.
* --bool2while — Uses a bool array for storage, and the 1 of 2 algorithm. Uses a while loop instead of a for loop.
* --bool6 — Uses a bool array for storage, and the 2 of 6 algorithm.
* --bool30 — Uses a bool array for storage, and the 8 of 30 algorithm.
* --ibool2 — Uses a bool array for storage, and the 1 of 2 algorithm. Uses inverted boolean logic.
* --ibool2while — Uses a bool array for storage, and the 1 of 2 algorithm. Uses inverted boolean logic. Uses a while loop instead of a for loop.
* --ibool6 — Uses a bool array for storage, and the 2 of 6 algorithm. Uses inverted boolean logic.
* --ibool30 — Uses a bool array for storage, and the 8 of 30 algorithm. Uses inverted boolean logic.
* --poolb2 — Uses a byte array from the array pool for storage, and the 1 of 2 algorithm.
* --poolb6 — Uses a byte array from the array pool for storage, and the 2 of 6 algorithm.
* --poolb30 — Uses a byte array from the array pool for storage, and the 8 of 30 algorithm.
* --poold2 — Uses a 32 bit array from the array pool for storage, and the 1 of 2 algorithm.
* --poold6 — Uses a 32 bit array from the array pool for storage, and the 2 of 6 algorithm.
* --poold30 — Uses a 32 bit array from the array pool for storage, and the 8 of 30 algorithm.
* --poolq2 — Uses a 64 bit array from the array pool for storage, and the 1 of 2 algorithm.
* --poolq6 — Uses a 64 bit array from the array pool for storage, and the 2 of 6 algorithm.
* --poolq30 — Uses a 64 bit array from the array pool for storage, and the 8 of 30 algorithm.
* --poolq30m — Uses a 64 bit array from the array pool for storage, and the 8 of 30 bitmasking algorithm.
* --rawb2 — Uses a directly allocated byte array for storage, and the 1 of 2 algorithm.
* --rawb6 — Uses a directly allocated byte array for storage, and the 2 of 6 algorithm.
* --rawb30 — Uses a directly allocated byte array for storage, and the 8 of 30 algorithm.
* --rawd2 — Uses a directly allocated 32 bit array for storage, and the 1 of 2 algorithm.
* --rawd6 — Uses a directly allocated 32 bit array for storage, and the 2 of 6 algorithm.
* --rawd30 — Uses a directly allocated 32 bit array for storage, and the 8 of 30 algorithm.
* --rawq2 — Uses  directly allocateda 64 bit array for storage, and the 1 of 2 algorithm.
* --rawq6 — Uses a directly allocated 64 bit array for storage, and the 2 of 6 algorithm.
* --rawq30 — Uses a directly allocated 64 bit array for storage, and the 8 of 30 algorithm.
* --rawq30m — Uses a directly allocated 64 bit array for storage, and the 8 of 30 bitmasking algorithm.


Any number of individual sieves can be specified.  In addition, you can specify a particular sieve property, and all sieves that have that property will be run.

* --bitarray — Sieves that use a bitarray for storage.
* --bool — Sieves that use a bool array for storage.
* --ibool — Sieves that use a bool array for storage, and invert the boolean logic.
* --pool — Sieves that use the array pool for storage.
* --raw — Sieves that manually allocate storage.
* --bytes — Sieves that allocate byte storage.
* --32bit — Sieves that allocate 32-bit storage.
* --64bit — Sieves that allocate 64-bit storage.
* --1of2 — Sieves that use the 1 of 2 algorithm.
* --2of6 — Sieves that use the 2 of 6 algorithm.
* --8of30 — Sieves that use the 8 of 30 algorithm.
* --bitmask — Sieves that use the bitmasking wheel algorithm.
* --parallel — Sieves that use a parallel algorithm. (None currently implemented)


And of course you can use `--all` to run all sieves.

### BenchmarkDotNet

You can also run BenchmarkDotNet benchmarks.  Options for that:

* --benchmark: Run benchmarks.  Add a -b value for a specific small benchmark.
* -b, --bench: Select a subgroup of benchmarks to run. Current sets are:
    * mod: Compare code that uses different operations for determining if a value is even or odd.
    * ref: Compare a ref struct implementation with its class version.
    * ofN: Compare a mod 2 algorithm with a mod 6 algorithm.
    * par: Compare a linear algorithm with its parallel alternate.

The sieves that are benchmarked depend on whether the commandline is set for V1 or V2 sieves.


## Results (Native)

Summary results for V1 sieves using .NET 5 vs V1 sieves using .NET 6 vs V2 sieves using .NET 6:

Change from .NET 5 to .NET 6:

|Name       | NET5 Loops |  NET6 Loops |     Diff   |
|-----------|------------|-------------|-----------:|
|original   |       3529 |        3545 |      0.5%  |
|standard   |       3667 |        3687 |      0.5%  |
|bool       |       5652 |        6285 |     11.2%  |
|ibool      |       7107 |        8926 |     25.6%  |
|dbool      |       7110 |        8928 |     25.6%  |
|raw        |       6321 |        6449 |      2.0%  |
|raw32      |       6599 |        6616 |      0.3%  |
|rawd       |       6375 |        6454 |      1.2%  |
|raw6       |       6107 |        6125 |      0.3%  |
|rawp       |       4248 |        4366 |      2.8%  |
|pool       |       6034 |        6658 |     10.3%  |
|pool2of6   |       9528 |        9468 |     -0.6%  |
|pool6p     |       5474 |        5531 |      1.0%  |
|pool30     |      12265 |       12821 |      4.5%  |
|pool30m    |      15817 |       17308 |      9.4%  |

Overall: +6.9%

V2 Performance:

|Name          |  Loops |
|--------------|--------|
|Bit2          |   3728 |
|Bit2While     |   3732 |
|Bit6          |   6973 |
|Bit30         |  10046 |
|Bool2         |   9150 |
|Bool2While    |  10356 |
|Bool6         |  10978 |
|Bool30        |  13135 |
|IBool2        |   9440 |
|IBool2While   |   9348 |
|IBool6        |  10759 |
|IBool30       |  13777 |
|PoolB2        |   5799 |
|PoolB6        |   9377 |
|PoolB30       |  11912 |
|PoolD2        |   7328 |
|PoolD6        |   9907 |
|PoolD30       |  13628 |
|PoolQ2        |   6511 |
|PoolQ6        |   9855 |
|PoolQ30       |  13498 |
|PoolQ30M      |  17665 |
|RawB2         |   6129 |
|RawB6         |   9040 |
|RawB30        |  10855 |
|RawD2         |   6150 |
|RawD6         |  10104 |
|RawD30        |  12019 |
|RawQ2         |   5800 |
|RawQ6         |   9700 |
|RawQ30        |  12045 |
|PoolQ30M      |  16204 |


V1 vs V2 performance (.NET 6):

|V1 Name    | V2 Name      |  V1 Loops |  V2 Loops  |    Diff  |
|-----------|--------------|-----------|------------|---------:|
|original   | Bit2While    |      3545 |      3732  |    5.3%  |
|standard   | Bit2While    |      3687 |      3732  |    1.2%  |
|bool       | Bool2        |      6285 |      9150  |   45.6%  |
|bool       | Bool2While   |      6285 |     10356  |   64.8%  |
|ibool      | IBool2       |      8926 |      9440  |    5.8%  |
|ibool      | IBool2While  |      8926 |      9348  |    4.7%  |
|dbool      | ---          |           |            |    0.0%  |
|raw        | RawB2        |      6449 |      6129  |   -5.0%  |
|raw32      | RawD2        |      6616 |      6150  |   -7.0%  |
|raw6       | RawB6        |      6125 |      9040  |   47.6%  |
|rawp       | ---          |           |            |    0.0%  |
|pool       | PoolQ2       |      6658 |      6511  |   -2.2%  |
|pool2of6   | PoolQ6       |      9468 |      9855  |    4.1%  |
|pool6p     | ---          |           |            |    0.0%  |
|pool30     | PoolQ30      |     12821 |     13498  |    5.3%  |
|pool30m    | PoolQ30M     |     17308 |     17665  |    2.1%  |

Overall: +11.2%


Docker runs will generate different results, depending on the type of sieve run.


## Results (BenchmarkDotNet)

### V1 Benchmark

```
BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19042.1348 (20H2/October2020Update)
AMD Ryzen 7 3700X, 1 CPU, 16 logical and 8 physical cores
.NET SDK=6.0.100
  [Host]     : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT
  DefaultJob : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT
```


|             Method | SieveSize |       Mean |   Error |  StdDev | Ratio |    Gen 0 |    Gen 1 |    Gen 2 | Allocated |
|------------------- |---------- |-----------:|--------:|--------:|------:|---------:|---------:|---------:|----------:|
|           Original |   1000000 | 1,339.4 us | 1.84 us | 1.44 us |  1.00 |   5.8594 |        - |        - |  62,593 B |
|           Standard |   1000000 | 1,342.4 us | 1.21 us | 1.01 us |  1.00 |   5.8594 |        - |        - |  62,593 B |
|               Bool |   1000000 |   743.4 us | 1.76 us | 1.65 us |  0.55 | 142.5781 | 142.5781 | 142.5781 | 500,105 B |
|       InvertedBool |   1000000 |   556.4 us | 2.37 us | 2.22 us |  0.41 | 142.5781 | 142.5781 | 142.5781 | 500,105 B |
| DirectInvertedBool |   1000000 |   556.0 us | 1.72 us | 1.61 us |  0.41 | 142.5781 | 142.5781 | 142.5781 | 500,105 B |
|            RawBits |   1000000 |   778.1 us | 2.30 us | 2.15 us |  0.58 |  19.5313 |  19.5313 |  19.5313 |  62,567 B |
|     ArrayPoolClass |   1000000 |   752.6 us | 2.63 us | 2.33 us |  0.56 |        - |        - |        - |      33 B |
|      ArrayPool2Of6 |   1000000 |   529.4 us | 3.19 us | 2.99 us |  0.39 |        - |        - |        - |      33 B |
|      ArrayPool6Par |   1000000 |   676.1 us | 2.48 us | 2.32 us |  0.50 |  36.1328 |        - |        - | 301,306 B |
|     ArrayPool8of30 |   1000000 |   387.2 us | 1.98 us | 1.75 us |  0.29 |        - |        - |        - |      96 B |
|    ArrayPool8of30M |   1000000 |   288.2 us | 1.30 us | 1.22 us |  0.21 |        - |        - |        - |     912 B |

### V2 Benchmark

```
BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19042.1348 (20H2/October2020Update)
AMD Ryzen 7 3700X, 1 CPU, 16 logical and 8 physical cores
.NET SDK=6.0.100
  [Host]     : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT
  DefaultJob : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT
```


|    Method | SieveSize |       Mean |   Error |  StdDev | Ratio |    Gen 0 |    Gen 1 |    Gen 2 | Allocated |
|---------- |---------- |-----------:|--------:|--------:|------:|---------:|---------:|---------:|----------:|
|      Bit2 |   1000000 | 1,345.1 us | 0.27 us | 0.24 us |  1.00 |   5.8594 |        - |        - |     61 KB |
| Bit2While |   1000000 | 1,351.1 us | 0.44 us | 0.41 us |  1.00 |   5.8594 |        - |        - |     61 KB |
|      Bit6 |   1000000 |   715.6 us | 0.39 us | 0.30 us |  0.53 |   6.8359 |   0.9766 |        - |     61 KB |
|     Bit30 |   1000000 |   496.1 us | 2.79 us | 2.61 us |  0.37 |   6.8359 |   0.9766 |        - |     61 KB |
|     Bool2 |   1000000 |   550.2 us | 5.07 us | 4.75 us |  0.41 | 142.5781 | 142.5781 | 142.5781 |    488 KB |
|     Bool6 |   1000000 |   455.6 us | 2.84 us | 2.52 us |  0.34 | 142.5781 | 142.5781 | 142.5781 |    488 KB |
|    Bool30 |   1000000 |   461.2 us | 2.89 us | 2.71 us |  0.34 | 142.5781 | 142.5781 | 142.5781 |    488 KB |
|    IBool2 |   1000000 |   537.5 us | 7.91 us | 7.40 us |  0.40 | 142.5781 | 142.5781 | 142.5781 |    488 KB |
|    IBool6 |   1000000 |   520.9 us | 5.46 us | 5.11 us |  0.39 | 142.5781 | 142.5781 | 142.5781 |    488 KB |
|   IBool30 |   1000000 |   453.7 us | 1.26 us | 1.18 us |  0.34 | 142.5781 | 142.5781 | 142.5781 |    488 KB |
|    PoolB2 |   1000000 |   860.8 us | 1.72 us | 1.44 us |  0.64 |   6.8359 |        - |        - |     64 KB |
|    PoolB6 |   1000000 |   531.9 us | 2.29 us | 1.91 us |  0.40 |   6.8359 |        - |        - |     64 KB |
|   PoolB30 |   1000000 |   418.5 us | 2.02 us | 1.79 us |  0.31 |   7.3242 |        - |        - |     64 KB |
|    PoolD2 |   1000000 |   729.2 us | 4.23 us | 3.96 us |  0.54 |   6.8359 |        - |        - |     64 KB |
|    PoolD6 |   1000000 |   503.2 us | 2.76 us | 2.58 us |  0.37 |   6.8359 |        - |        - |     64 KB |
|   PoolD30 |   1000000 |   367.2 us | 0.25 us | 0.21 us |  0.27 |   7.3242 |        - |        - |     64 KB |
|    PoolQ2 |   1000000 |   730.3 us | 1.20 us | 1.12 us |  0.54 |   6.8359 |        - |        - |     64 KB |
|    PoolQ6 |   1000000 |   508.6 us | 2.77 us | 2.59 us |  0.38 |   6.8359 |        - |        - |     64 KB |
|   PoolQ30 |   1000000 |   367.8 us | 1.61 us | 1.51 us |  0.27 |   7.3242 |        - |        - |     64 KB |
|  PoolQ30M |   1000000 |   292.0 us | 0.73 us | 0.64 us |  0.22 |   7.8125 |   1.4648 |        - |     65 KB |
|     RawB2 |   1000000 |   818.7 us | 2.84 us | 2.65 us |  0.61 |  19.5313 |  19.5313 |  19.5313 |     61 KB |
|     RawB6 |   1000000 |   567.7 us | 3.35 us | 3.14 us |  0.42 |  19.5313 |  19.5313 |  19.5313 |     61 KB |
|    RawB30 |   1000000 |   461.5 us | 2.28 us | 2.13 us |  0.34 |  19.5313 |  19.5313 |  19.5313 |     61 KB |
|     RawD2 |   1000000 |   825.3 us | 6.69 us | 6.26 us |  0.61 |  19.5313 |  19.5313 |  19.5313 |     61 KB |
|     RawD6 |   1000000 |   511.8 us | 3.69 us | 3.45 us |  0.38 |  19.5313 |  19.5313 |  19.5313 |     61 KB |
|    RawD30 |   1000000 |   419.4 us | 2.55 us | 2.26 us |  0.31 |  19.5313 |  19.5313 |  19.5313 |     61 KB |
|     RawQ2 |   1000000 |   877.7 us | 4.17 us | 3.90 us |  0.65 |  19.5313 |  19.5313 |  19.5313 |     61 KB |
|     RawQ6 |   1000000 |   525.3 us | 0.81 us | 0.72 us |  0.39 |  19.5313 |  19.5313 |  19.5313 |     61 KB |
|    RawQ30 |   1000000 |   429.0 us | 3.36 us | 3.15 us |  0.32 |  19.5313 |  19.5313 |  19.5313 |     61 KB |
|   RawQ30M |   1000000 |   308.2 us | 2.88 us | 2.70 us |  0.23 |  19.5313 |  19.5313 |  19.5313 |     62 KB |

