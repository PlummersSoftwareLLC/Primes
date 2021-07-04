# VB.NET solution by jimbojim1997

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is a VB.NET translation of davepl's C# implementation with minor differences.
1. The ```PrimeSieve``` implementation is in a separate class to the benchmarking code.
2. The benchmark uses a ```System.Diagnostics.Stopwatch``` to measure the execution time rather than a ```DateTime```.
3. The ```_knownPrimes``` dictionary is now ```Shared``` so it isn't re-instantiated for each test.
4. Method names changed to match VB convention.
5. Results reported from within ```Main``` rather than a separate method.


## Run instructions

### .NET5 runtime

1. Install the .NET5 runtime: https://dotnet.microsoft.com/download
2. From within the solution folder run: ```dotnet run -c Release```

### Docker

From within the solution folder run:

    docker build . -t primesievevb
    docker run primesievevb


## Output

Intel Core i7-9700K @ 3.6GHz, Windows 10 Pro 20H2. Results for 10, 100, 1,000, 10,000, 100,000 1,000,000 iterations:

    jimbojim1997;122639480;5.00002;1;algorithm=base,faithful=yes,bits=1
    jimbojim1997;35720565;5.00002;1;algorithm=base,faithful=yes,bits=1
    jimbojim1997;3625175;5.00002;1;algorithm=base,faithful=yes,bits=1
    jimbojim1997;305348;5.00002;1;algorithm=base,faithful=yes,bits=1
    jimbojim1997;26601;5.00014;1;algorithm=base,faithful=yes,bits=1
    jimbojim1997;2405;5.00127;1;algorithm=base,faithful=yes,bits=1
