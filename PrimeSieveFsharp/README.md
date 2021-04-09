# Primes | F# Implementation

Port of the original C++ prime number sieve to F# (thus, non-idomatic F# - for science!). 

Similar to the C# version in that it's what the compiler gives you by default. No optimisations or performance tweaks...yet.

## Requirements
- [dotnet core >= 3.1](https://dotnet.microsoft.com/download/dotnet-core)

## Run
- run ```dotnet run -c Release```

## Performance examples

CPU Used: 3700x (4.3GHz boost)

### C++
```
Passes: 10480, Time: 5.000000, Avg: 0.000477, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
```

### F# [Recursion](PrimeSieveFsharp_Recursion)
```
Passes: 5585, Time: 5.000537, Avg: 0.000895, Limit: 1000000, Count: 78498, Valid: true
```

### F# [Port from C++](PrimeSieveFsharp_Port)
```
Passes: 5248, Time: 5.000992, Avg: 0.000953, Limit: 1000000, Count: 78498, Valid: true
```

### C#
```
Passes: 3545, Time: 5.0012068, Avg: 0.0014107776586741892, Limit: 1000000, Count: 78498, Valid: True
```
_Note: Ran the updated C# version that also matches the latest C++ version where the bitArray length = sieveSize. 

Perf improved but this still seems odd, especially compared with F#. However, further runs show the same. Atm I assume the difference is due to compiler defaults & coding styles._

### Haskell
VectorBoolUnchecked ver (Windows no LLVM)
```
Passes: 11342, Time: 5.000062, Avg: 0.000441, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: True
```

### Go
```
Passes: 7508, Time: 5000622400, Avg: 0.000000, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```

### Python
```
Passes: 29, Time: 5.1648879, Avg: 0.1780995827586207, Limit: 1000000, Count: 78498, Valid: True
```