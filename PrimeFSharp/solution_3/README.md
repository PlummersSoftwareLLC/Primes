# F# Solution by dmannock

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Port of the original C++ prime number sieve to F# using recursion. 

Similar to the C# version in that it's what the compiler gives you by default and builds to the same targets.

## Run instructions
- [.NET >= 5.0](https://dotnet.microsoft.com/download/dotnet/5.0)
- run ```dotnet run -c Release```

## Docker
A Dockerfile has been provided.

## Output
```
dmannock_fsharp_recursion;9839;5.000479;1;algorithm=base,faithful=yes,bits=1
```

## Original Output

CPU Used: 3700x (4.3GHz boost)

### F# [Recursion](PrimeSieveFsharp_Recursion) (_this solution_)
```
Passes: 9839, Time: 5.000479, Avg: 0.000508, Limit: 1000000, Count: 78498, Valid: true
```

### F# [Port from C++](PrimeSieveFsharp_Port)
```
Passes: 9669, Time: 5.000318, Avg: 0.000517, Limit: 1000000, Count: 78498, Valid: true
```

### C# (raw bits)
```
Passes: 6201, Time: 5.00026 s, Per Loop: 0.806322 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
```

### C# (bool array)
```
Passes: 5106, Time: 5.00088 s, Per Loop: 0.979240 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid
```

### C# (original)
```
Passes: 3545, Time: 5.0012068, Avg: 0.0014107776586741892, Limit: 1000000, Count: 78498, Valid: True
```

### C++
```
Passes: 10480, Time: 5.000000, Avg: 0.000477, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
```

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