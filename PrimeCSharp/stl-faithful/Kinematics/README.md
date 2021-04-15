# Prime Sieve Algorithms

## C# solution by @Kinematics

## Building

This is a collection of prime sieve algorithms implemented in C#.  It uses .NET 5, and can be compiled using Visual Studio 16.9 or later.

To compile from the commandline, just use `dotnet publish -c Release` from within the solution directory.  The output will be placed in `.\bin\Release\net5.0\publish\`

## Run instructions

If you run from Visual Studio, you can put commandline arguments in the debug tab of the project properties. Right-click on the project, select `Properties`, and under `Debug` you can enter the commandline arguments you want to be included when run in the `Application arguments` box.

## Options

Options you can set from the commandline:

* -s, --size: The size of the sieve to run. Must be an int larger than 2.
* -t, --time: How many seconds to run the process.
* -v, --verbose: Display the primes that were found once the run is complete.
* -m, --multi: Run multithreaded, with each thread running its own instance of the sieve.
* --threads: How many threads to use when running multithreaded. Defaults to the number of processors on the system.
* --pthreads: How many threads to use when running parallel implementations. No effect on non-parallel versions.

You can also run BenchmarkDotNet benchmarks.  Options for that:

* --benchmark: Run benchmarks.
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

@Kinematics: Starting (original)...
Passes: 2680, Time: 5.00174 s, Per Loop: 1.866045 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True

@Kinematics: Starting (standard)...
Passes: 3691, Time: 5.00157 s, Per Loop: 1.354917 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True

@Kinematics: Starting (bool array)...
Passes: 5207, Time: 5.00002 s, Per Loop: 0.960246 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True

@Kinematics: Starting (inverted bool array)...
Passes: 6370, Time: 5.00017 s, Per Loop: 0.784929 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True

@Kinematics: Starting (direct access inverted bool array)...
Passes: 9006, Time: 5.00018 s, Per Loop: 0.555185 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True

@Kinematics: Starting (raw bits)...
Passes: 6227, Time: 5.00053 s, Per Loop: 0.802955 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True

@Kinematics: Starting (raw bits uint)...
Passes: 6422, Time: 5.00064 s, Per Loop: 0.778574 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True

@Kinematics: Starting (raw bits direct)...
Passes: 6188, Time: 5.00076 s, Per Loop: 0.808016 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True

@Kinematics: Starting (array pool)...
Passes: 6132, Time: 5.00039 s, Per Loop: 0.815395 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True

@Kinematics: Starting (array pool - ref struct)...
Passes: 6056, Time: 5, MS per Loop: 0.825627, Sieve Size: 1000000, Primes Found: 78498, Valid: True

@Kinematics: Starting (array pool [2 of 6])...
Passes: 9746, Time: 5.0002 s, Per Loop: 0.513031 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True

@Kinematics: Starting (parallel array pool [2 of 6])...
Passes: 6811, Time: 5.0004 s, Per Loop: 0.734107 ms, Sieve Size: 1000000, Thread Count: 1, Parallel Thread Count: 3, Primes Found: 78498, Valid: True

@Kinematics: Starting (raw bits parallel version)...
Passes: 4627, Time: 5.00086 s, Per Loop: 1.080614 ms, Sieve Size: 1000000, Thread Count: 1, Parallel Thread Count: 3, Primes Found: 78498, Valid: True

NB: Parallel versions don't start outperforming linear versions until 10,000,000 sieve size.

@Kinematics: Starting (array pool [8 of 30])...
Passes: 7799, Time: 5.00053 s, Per Loop: 0.641108 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True


And results of running --benchmark

```
BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
AMD Ryzen 7 3700X, 1 CPU, 16 logical and 8 physical cores
.NET Core SDK=5.0.201
  [Host]     : .NET Core 5.0.5 (CoreCLR 5.0.521.16609, CoreFX 5.0.521.16609), X64 RyuJIT
  DefaultJob : .NET Core 5.0.5 (CoreCLR 5.0.521.16609, CoreFX 5.0.521.16609), X64 RyuJIT


|             Method | SieveSize |       Mean |    Error |   StdDev | Ratio |    Gen 0 |    Gen 1 |    Gen 2 | Allocated |
|------------------- |---------- |-----------:|---------:|---------:|------:|---------:|---------:|---------:|----------:|
|           Original |   1000000 | 1,393.0 us |  6.28 us |  5.57 us |  1.00 |   5.8594 |        - |        - |   62592 B |
|           Standard |   1000000 | 1,394.2 us |  9.99 us |  8.86 us |  1.00 |   5.8594 |        - |        - |   62592 B |
|               Bool |   1000000 |   998.0 us | 15.34 us | 14.35 us |  0.72 | 142.5781 | 142.5781 | 142.5781 |  500056 B |
|       InvertedBool |   1000000 |   817.6 us |  9.30 us |  8.24 us |  0.59 | 142.5781 | 142.5781 | 142.5781 |  500056 B |
| DirectInvertedBool |   1000000 |   577.9 us |  8.38 us |  7.84 us |  0.41 | 142.5781 | 142.5781 | 142.5781 |  500056 B |
|            RawBits |   1000000 |   827.2 us |  8.63 us |  8.07 us |  0.59 |  19.5313 |  19.5313 |  19.5313 |   62560 B |
|     ArrayPoolClass |   1000000 |   816.4 us |  1.86 us |  1.55 us |  0.59 |        - |        - |        - |      32 B |
|      ArrayPool2Of6 |   1000000 |   505.2 us |  2.05 us |  1.81 us |  0.36 |        - |        - |        - |      32 B |
|      ArrayPool6Par |   1000000 |   714.5 us |  1.82 us |  1.61 us |  0.51 |  35.1563 |        - |        - |  296369 B |
```


