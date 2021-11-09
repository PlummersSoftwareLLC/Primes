# F# "Unpeeled" Solution by GordonBGood

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This algorithm is similar to the Rust "striped" version, just differing in the order in which the culling/marking of composite number representation bits is done but with all marking culling done in inner "loops" as per found base prime value factors in the outer "loop" as per the `base` specification.  It is implemented functionally with recursive function loops;  no direct mutation is used other than for the contents of the sieving buffer array.

Although use of pointer memory access is not recommended other than for the purposed of foreign function marshalling, The C# striped solution_4 uses pointer math for speed, so it is tried here for some small benefit; the trouble is that while C# no longer de-tunes pointer operations (formerly to discourage their common use other than for marshalling), they still aren't very efficient in F# as they break the functional language paradigm because they use mutability.  However, since there is some small benefit, they are used here.

The de-tuning may have been eased in newer dotnet versions such that the program runs much faster run locally on my machine than using the Docker image as per the following results:

## Run instructions
- [.NET >= 5.0](https://dotnet.microsoft.com/download/dotnet/5.0)
- run ```dotnet run -c=Release```

## Docker
A Dockerfile has been provided.

## Output locally on Intel SkyLake i5-6500 (3.6 Ghz when single-threading as here)
```
Passes: 11249, Time: 5.000319, Avg: 0.000538, Limit: 1000000, Count: 78498, Valid: true
```

## Original Docker Output

CPU Used:  Intel SkyLake i5-6500 (3.6 Ghz when single-threading as here)

### F# [Unpeeled](PrimeSieveFsharp_Unpeeling) (_this solution_)
```
Passes: 9192, Time: 5.00003, Avg: 0.00054, Limit: 1000000, Count: 78498, Valid: true
```

### F# [Recursion](PrimeSieveFsharp_Recursion)
```
Passes: 4171, Time: 5.000319, Avg: 0.00120, Limit: 1000000, Count: 78498, Valid: true
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

This version implementation is similar to the Rust "striped" version, which is categorized as `faithful base` because it has an outer loop to find the odd base prime values and (in this case) inner loops to mark the composite number representations that are multiples of the found outer loop base prime values.  These implementations are faster than the common implementations because the eight outer loops just set up the mask and start byte index for the inner loops which do all the work but with a simpler loop due to using a constant mask value and indexing by bytes rather than "bit-twiddling".

The F# outer loop code finding each of the base prime factor values starts at line 35 and the code that implements these double loops is in lines 42 to 52.  There is no need to elide automatic array bounds checking because pointer access is used.
