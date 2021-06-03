# Prime Sieve Algorithms

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## C# solution by @Kinematics

This is a collection of prime sieve algorithms implemented in C#.  It uses .NET 5, and can be compiled using Visual Studio 16.9 or later.

## Building

To compile from the commandline, just use `dotnet build -c Release` or `dotnet publish -c Release` from within the solution directory.  The output will be placed in `.bin\x64\Release\net5.0` or `.\bin\Release\net5.0\publish\`, respectively.

## Run instructions

If you run from Visual Studio, you can put commandline arguments in the debug tab of the project properties. Right-click on the project, select `Properties`, and under `Debug` you can enter the commandline arguments you want to be included when run in the `Application arguments` box.

You can run it without the debugger attaching to the process by hitting Ctrl-F5.

## Options

Options you can set from the commandline:

* -s, --size: The size of the sieve to run. Must be at least 10. Default: 1000000
* -t, --time: How many seconds to run the process. Default: 5 seconds
* -v, --verbose: Display the primes that were found once the run is complete. Defaults to off.
* -m, --multi: Run multithreaded, with each thread running its own instance of the sieve. Defaults to off.
* --threads: How many threads to use when running multithreaded. Defaults to the number of processors on the system.
* --pthreads: How many threads to use when running parallel implementations. No effect on non-parallel versions.

You can also run BenchmarkDotNet benchmarks.  Options for that:

* --benchmark: Run benchmarks.  Add a -b value for a specific small benchmark.
* -b, --bench: Select a subgroup of benchmarks to run. Current sets are:
    * mod: Compare code that uses different operations for determining if a value is even or odd.
    * ref: Compare a ref struct implementation with its class version.
    * ofN: Compare a mod 2 algorithm with a mod 6 algorithm.
    * par: Compare a linear algorithm with its parallel alternate.

And finally, you can choose which implementation to run:

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

If no version is specified, it runs the 'Standard' implementation.


## Output

Results that I get, running these on a Ryzen 3700X.

```
@Kinematics: Starting (original)...
Passes: 2919, Time: 5.00151 s, Per Loop: 1.713258 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_original;2919;5.00151;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (standard)...
Passes: 3678, Time: 5.00021 s, Per Loop: 1.359434 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_standard;3678;5.00021;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (bool array)...
Passes: 5102, Time: 5.00011 s, Per Loop: 0.980008 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_bool;5102;5.00011;1;algorithm=base,faithful=yes

@Kinematics: Starting (inverted bool array)...
Passes: 6313, Time: 5.00056 s, Per Loop: 0.792016 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_ibool;6313;5.00056;1;algorithm=base,faithful=yes

@Kinematics: Starting (direct access inverted bool array)...
Passes: 8964, Time: 5.00047 s, Per Loop: 0.557787 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_dbool;8964;5.00047;1;algorithm=base,faithful=yes

@Kinematics: Starting (raw bits)...
Passes: 6187, Time: 5.00047 s, Per Loop: 0.808146 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_raw;6187;5.00047;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (raw bits uint)...
Passes: 6366, Time: 5.00003 s, Per Loop: 0.785423 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_raw32;6366;5.00003;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (raw bits direct)...
Passes: 6416, Time: 5.00043 s, Per Loop: 0.779302 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_rawd;6416;5.00043;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (raw bits [2 of 6])...
Passes: 6103, Time: 5.00023 s, Per Loop: 0.819269 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_raw6;6103;5.00023;1;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (raw bits parallel version)...
Passes: 4207, Time: 5.00048 s, Per Loop: 1.188495 ms, Sieve Size: 1000000, Thread Count: 1, Parallel Thread Count: 16, Primes Found: 78498, Valid: True
kinematics_rawp;4207;5.00048;16;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (array pool)...
Passes: 6084, Time: 5.00053 s, Per Loop: 0.821828 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool;6084;5.00053;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (array pool [2 of 6])...
Passes: 9670, Time: 5.00003 s, Per Loop: 0.517063 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool2of6;9670;5.00003;1;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (parallel array pool [2 of 6])...
Passes: 5467, Time: 5.00014 s, Per Loop: 0.914578 ms, Sieve Size: 1000000, Thread Count: 1, Parallel Thread Count: 16, Primes Found: 78498, Valid: True
kinematics_pool6p;5467;5.00014;16;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (array pool [8 of 30])...
Passes: 12467, Time: 5.00035 s, Per Loop: 0.401059 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool30;12467;5.00035;1;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (array pool [8 of 30] with bitmasking)...
Passes: 15611, Time: 5.00008 s, Per Loop: 0.320287 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool30m;15611;5.00008;1;algorithm=wheel,faithful=yes,bits=1
```

NB: Parallel versions don't start outperforming linear versions until 10,000,000 sieve size.


And results of running --benchmark

```
BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
AMD Ryzen 7 3700X, 1 CPU, 16 logical and 8 physical cores
.NET Core SDK=5.0.201
  [Host]     : .NET Core 5.0.5 (CoreCLR 5.0.521.16609, CoreFX 5.0.521.16609), X64 RyuJIT
  DefaultJob : .NET Core 5.0.5 (CoreCLR 5.0.521.16609, CoreFX 5.0.521.16609), X64 RyuJIT


|             Method | SieveSize |       Mean |   Error |  StdDev | Ratio |    Gen 0 |    Gen 1 |    Gen 2 | Allocated |
|------------------- |---------- |-----------:|--------:|--------:|------:|---------:|---------:|---------:|----------:|
|           Original |   1000000 | 1,381.3 us | 2.37 us | 2.10 us |  1.01 |   5.8594 |        - |        - |   62592 B |
|           Standard |   1000000 | 1,361.7 us | 4.89 us | 4.57 us |  1.00 |   5.8594 |        - |        - |   62592 B |
|               Bool |   1000000 |   988.7 us | 7.98 us | 7.47 us |  0.73 | 142.5781 | 142.5781 | 142.5781 |  500056 B |
|       InvertedBool |   1000000 |   794.6 us | 4.71 us | 4.41 us |  0.58 | 142.5781 | 142.5781 | 142.5781 |  500056 B |
| DirectInvertedBool |   1000000 |   565.9 us | 2.02 us | 1.79 us |  0.42 | 142.5781 | 142.5781 | 142.5781 |  500056 B |
|            RawBits |   1000000 |   806.8 us | 2.69 us | 2.39 us |  0.59 |  19.5313 |  19.5313 |  19.5313 |   62560 B |
|     ArrayPoolClass |   1000000 |   821.6 us | 2.67 us | 2.50 us |  0.60 |        - |        - |        - |      32 B |
|      ArrayPool2Of6 |   1000000 |   519.0 us | 3.33 us | 2.78 us |  0.38 |        - |        - |        - |      32 B |
|      ArrayPool6Par |   1000000 |   722.8 us | 4.36 us | 3.87 us |  0.53 |  35.1563 |        - |        - |  295840 B |
|     ArrayPool8of30 |   1000000 |   398.9 us | 1.98 us | 1.86 us |  0.29 |        - |        - |        - |      96 B |
|    ArrayPool8of30M |   1000000 |   320.7 us | 0.81 us | 0.67 us |  0.24 |        - |        - |        - |     912 B |
```


