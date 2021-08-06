# F# Solution by GordonBGood

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Algorithm is similar to Rust "striped" version (which I call "Loop Unpeeling") but implemented functionally with recursive function loops;  no direct mutation is used other than for the contents of the sieving buffer array.

## Run instructions
- [.NET >= 5.0](https://dotnet.microsoft.com/download/dotnet/5.0)
- run ```dotnet run -c Release```

## Docker
A Dockerfile has been provided.

## Output on Intel SkyLake i5-6500 (3.6 Ghz when single-threading as here)
```
GordonBGood_unpeeling;8465;5.00028;1;algorithm=base,faithful=yes,bits=1
```

## Original Output

CPU Used:  Intel SkyLake i5-6500 (3.6 Ghz when single-threading as here)

### F# [Unpeeling](PrimeSieveFsharp_Unpeeling) (_this solution_)
```
Passes: 8543, Time: 5.00022, Avg: 0.00059, Limit: 1000000, Count: 78498, Valid: true
```

### F# [Recursion](PrimeSieveFsharp_Recursion)
```
Passes: 4171, Time: 5.00013, Avg: 0.00120, Limit: 1000000, Count: 78498, Valid: true
```

### F# [Port from C++](PrimeSieveFsharp_Port)
```
Passes: 4620, Time: 5.00065, Avg: 0.00108, Limit: 1000000, Count: 78498, Valid: true
```

### C# (raw bits)
```
Passes: 3850, Time: 5.00089 s, Per Loop: 0.00130 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid: True
```

### C# (bool array)
```
Passes: 2495, Time: 5.00003 s, Per Loop: 0.00200 ms, Sieve Size: 1000000, Thread Count: 1, Primes Found: 78498, Valid
```

### C# (original)
```
Passes: 1058, Time: 5.00070, Avg: 0.00473, Limit: 1000000, Count: 78498, Valid: True
```

### Rust (striped)
```
Passes: 11141, Time: 5.00015, Avg: 0.00045, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
```

### C++
```
Passes: 6577, Time: 5.00059, Avg: 0.00076, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
```

### Go (bit solution)
```
Passes: 5789, Time: 5.00153, Avg: 0.00086, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```

### Python (original)
```
Passes: 18, Time: 5.09651, Avg: 0.28314, Limit: 1000000, Count: 78498, Valid: True
```
## Notes

This version implementation is the same as the Rust "striped" version, which is categorized as `faithful base` because it has an outer loop to find the odd base prime values and (in this case) inner loops to mark the composite number representations that are multiples of the found outer loop base prime values.  These implementations are faster than the common implementations because the eight outer loops just set up the mask and start byte index for the inner loops which do all the work but with a simpler loop due to using a constant mask value and indexing by bytes rather than "bit-twiddling".

The F# code that implements these double loops is in lines 33 to 39.  This code also elides the normal compiler automatic array bounds check by including the array length as part of the loop limit condition at code line 37 so that the compiler realizes that the bounds check is already done as part of the loop limit.
