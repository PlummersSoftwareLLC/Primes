# Primes | F# Port

Port of the original C++ prime number sieve to F# (thus, non-idomatic F# - for science!). 

Similar to the C# version in that it's what the compiler gives you by default. No optimisations or performance tweaks...yet.

## Requirements
- [dotnet core >= 3.1](https://dotnet.microsoft.com/download/dotnet-core)

## Run
- run ```dotnet run -c Release```

## Performance examples

### F#
```
Passes: 5432, Time: 5.000246, Avg: 0.000921, Limit: 1000000, Count: 78498, Valid: true
```

### C#
```
Passes: 3545, Time: 5.0012068, Avg: 0.0014107776586741892, Limit: 1000000, Count: 78498, Valid: True
```
_Note: Ran the updated C# version that also matches the latest C++ version where the bitArray length = sieveSize. 

Perf improved but this still seems odd, especially compared with F#. However, further runs show the same. Atm I assume the difference is due to compiler defaults & coding styles._

### C++
```
Passes: 9905, Time: 5.000000, Avg: 0.000505, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
```

### Python
```
Passes: 29, Time: 5.1648879, Avg: 0.1780995827586207, Limit: 1000000, Count: 78498, Valid: True
```

### More F# runs to see variances
```
Passes: 5432, Time: 5.000246, Avg: 0.000921, Limit: 1000000, Count: 78498, Valid: true
Passes: 5352, Time: 5.000009, Avg: 0.000934, Limit: 1000000, Count: 78498, Valid: true
Passes: 5343, Time: 5.000084, Avg: 0.000936, Limit: 1000000, Count: 78498, Valid: true
Passes: 5339, Time: 5.000284, Avg: 0.000937, Limit: 1000000, Count: 78498, Valid: true
Passes: 5350, Time: 5.000217, Avg: 0.000935, Limit: 1000000, Count: 78498, Valid: true
Passes: 5338, Time: 5.000298, Avg: 0.000937, Limit: 1000000, Count: 78498, Valid: true
Passes: 5314, Time: 5.000626, Avg: 0.000941, Limit: 1000000, Count: 78498, Valid: true
Passes: 5333, Time: 5.000419, Avg: 0.000938, Limit: 1000000, Count: 78498, Valid: true
Passes: 5318, Time: 5.000350, Avg: 0.000940, Limit: 1000000, Count: 78498, Valid: true
Passes: 5344, Time: 5.000453, Avg: 0.000936, Limit: 1000000, Count: 78498, Valid: true
```
