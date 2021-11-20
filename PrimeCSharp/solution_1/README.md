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

Docker: Run the Docker build process from within Visual Studio, or use the `docker build` command from the commandline.

## Run instructions

If you run from Visual Studio, you can put commandline arguments in the debug tab of the project properties. Right-click on the project, select `Properties`, and under `Debug` you can enter the commandline arguments in the debug launch profiles.

You can run it without the debugger attaching to the process by hitting Ctrl-F5.

To run the Docker container, use the command `docker run -rm primecsharps1 [options]`, using the options provided below.

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


## Results (Native)

Results that I get, running these on a Ryzen 3700X.

```
@Kinematics: Starting (original)...
Passes: 3545, Time: 5.00124 s, Per Loop: 1.410719 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_original;3545;5.00124;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (standard)...
Passes: 3687, Time: 5.00099 s, Per Loop: 1.356116 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_standard;3687;5.00099;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (bool array)...
Passes: 6285, Time: 5.00022 s, Per Loop: 0.795545 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_bool;6285;5.00022;1;algorithm=base,faithful=yes,bits=8

@Kinematics: Starting (inverted bool array)...
Passes: 8926, Time: 5.00009 s, Per Loop: 0.560161 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_ibool;8926;5.00009;1;algorithm=base,faithful=yes,bits=8

@Kinematics: Starting (direct access inverted bool array)...
Passes: 8928, Time: 5.00052 s, Per Loop: 0.560036 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_dbool;8928;5.00052;1;algorithm=base,faithful=yes,bits=8

@Kinematics: Starting (raw bits)...
Passes: 6449, Time: 5.00052 s, Per Loop: 0.775314 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_raw;6449;5.00052;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (raw bits uint)...
Passes: 6616, Time: 5.00009 s, Per Loop: 0.755744 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_raw32;6616;5.00009;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (raw bits direct)...
Passes: 6454, Time: 5.00025 s, Per Loop: 0.774713 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_rawd;6454;5.00025;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (raw bits [2 of 6])...
Passes: 6125, Time: 5.00021 s, Per Loop: 0.816327 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_raw6;6125;5.00021;1;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (raw bits parallel version)...
Passes: 4366, Time: 5.00071 s, Per Loop: 1.145213 ms, Sieve Size: 1000000, Thread Count: 1, Parallel Thread Count: 16, Primes Found: 78498, Valid: True
kinematics_rawp;4366;5.00071;16;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (array pool)...
Passes: 6658, Time: 5.00036 s, Per Loop: 0.750976 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool;6658;5.00036;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (array pool [2 of 6])...
Passes: 9468, Time: 5.00034 s, Per Loop: 0.528095 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool2of6;9468;5.00034;1;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (parallel array pool [2 of 6])...
Passes: 5531, Time: 5.0002 s, Per Loop: 0.903996 ms, Sieve Size: 1000000, Thread Count: 1, Parallel Thread Count: 16, Primes Found: 78498, Valid: True
kinematics_pool6p;5531;5.0002;16;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (array pool [8 of 30])...
Passes: 12821, Time: 5.0002 s, Per Loop: 0.389985 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool30;12821;5.0002;1;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (array pool [8 of 30] with bitmasking)...
Passes: 17308, Time: 5.00002 s, Per Loop: 0.288884 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool30m;17308;5.00002;1;algorithm=wheel,faithful=yes,bits=1
```

NB: Parallel versions don't start outperforming linear versions until 10,000,000 sieve size.


## Results (Docker)

Docker results are a bit different from native results.  Bitarrays are slower, at about 2/3 of the speed of their native counterparts.

Bool arrays and raw allocations are about the same between Docker and native runs.  These would be the best to use for comparisons.

Parallel algorithms tank when run in Docker, only getting about 10% of their native counterparts (which weren't very good to start with).

And the 8 of 30 algorithms (including the bitmasking version) are significantly below native results (1/3 to 1/2 native speed).


@Kinematics: Starting (original)...
Passes: 2293, Time: 5.00169 s, Per Loop: 2.180986 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_original;2293;5.00169;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (standard)...
Passes: 2470, Time: 5.00116 s, Per Loop: 2.024696 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_standard;2470;5.00116;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (bool array)...
Passes: 6134, Time: 5.00074 s, Per Loop: 0.815129 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_bool;6134;5.00074;1;algorithm=base,faithful=yes,bits=8

@Kinematics: Starting (inverted bool array)...
Passes: 8937, Time: 5.00061 s, Per Loop: 0.559472 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_ibool;8937;5.00061;1;algorithm=base,faithful=yes,bits=8

@Kinematics: Starting (direct access inverted bool array)...
Passes: 8834, Time: 5.00039 s, Per Loop: 0.565995 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_dbool;8834;5.00039;1;algorithm=base,faithful=yes,bits=8

@Kinematics: Starting (raw bits)...
Passes: 6052, Time: 5.0007 s, Per Loop: 0.826173 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_raw;6052;5.0007;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (raw bits uint)...
Passes: 6316, Time: 5.00079 s, Per Loop: 0.791640 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_raw32;6316;5.00079;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (raw bits direct)...
Passes: 6063, Time: 5.00034 s, Per Loop: 0.824674 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_rawd;6063;5.00034;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (raw bits [2 of 6])...
Passes: 5983, Time: 5.00047 s, Per Loop: 0.835701 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_raw6;5983;5.00047;1;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (raw bits parallel version)...
Passes: 596, Time: 5.00006 s, Per Loop: 8.389262 ms, Sieve Size: 1000000, Thread Count: 1, Parallel Thread Count: 16, Primes Found: 78498, Valid: True
kinematics_rawp;596;5.00006;16;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (array pool)...
Passes: 6451, Time: 5.00041 s, Per Loop: 0.775074 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool;6451;5.00041;1;algorithm=base,faithful=yes,bits=1

@Kinematics: Starting (array pool [2 of 6])...
Passes: 8972, Time: 5.00019 s, Per Loop: 0.557289 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool2of6;8972;5.00019;1;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (parallel array pool [2 of 6])...
Passes: 595, Time: 5.00746 s, Per Loop: 8.415126 ms, Sieve Size: 1000000, Thread Count: 1, Parallel Thread Count: 16, Primes Found: 78498, Valid: True
kinematics_pool6p;595;5.00746;16;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (array pool [8 of 30])...
Passes: 6618, Time: 5.00011 s, Per Loop: 0.755515 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool30;6618;5.00011;1;algorithm=wheel,faithful=yes,bits=1

@Kinematics: Starting (array pool [8 of 30] with bitmasking)...
Passes: 6160, Time: 5.00068 s, Per Loop: 0.811688 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
kinematics_pool30m;6160;5.00068;1;algorithm=wheel,faithful=yes,bits=1


## Results (BenchmarkDotNet)

And results of running --benchmark

```
BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19042.1348 (20H2/October2020Update)
AMD Ryzen 7 3700X, 1 CPU, 16 logical and 8 physical cores
.NET SDK=6.0.100
  [Host]     : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT
  DefaultJob : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT


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
|    ArrayPool8of30M |   1000000 |   288.2 us | 1.30 us | 1.22 us |  0.21 |        - |        - |        - |     912 B |```


