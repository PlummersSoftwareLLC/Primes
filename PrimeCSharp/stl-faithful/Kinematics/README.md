# Prime Sieve Algorithms

## Building

This is a collection of prime sieve algorithms implemented in C#.  It uses .NET 5, and can be compiled using Visual Studio 16.9 or later.

To compile from the commandline, just use `dotnet publish -c Release` from within the solution directory.  The output will be placed in `.\bin\Release\net5.0\publish\`

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
* --pool6par — The array pool using mod 6 filters, parallelized.
* --parallel — The rawbits version, parallelized.
* --pool30 — Uses an array pool, and calculations using mod 30 filters.

If no version is specified, it runs the 'Standard' implementation.


