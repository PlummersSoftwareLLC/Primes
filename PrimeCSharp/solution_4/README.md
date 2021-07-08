
# C# solution by hamarb123

![Algorithm](https://img.shields.io/badge/Algorithm-base-green) ![Faithfulness](https://img.shields.io/badge/Faithful-yes-green) ![Parallelism](https://img.shields.io/badge/Parallel-no-green) ![Bit count](https://img.shields.io/badge/Bits-8-green) ![Bit count](https://img.shields.io/badge/Bits-1-green)

# Speed
On my MBP 15in 2018 with i9 8950HK (in docker), the results are as follows for 5 seconds:
```
hamarb123_optimized,unmanaged,vectorized,onebit => 6837 repeats
hamarb123_optimized,unmanaged,onebit => 6112 repeats
hamarb123_optimized,unmanaged,vectorized => 7458 repeats
hamarb123_optimized,unmanaged => 3017 repeats
hamarb123_optimized,vectorized => 6172 repeats
hamarb123_optimized => 2993 repeats
hamarb123_standard => 2490 repeats
```

# Variants:
- Method 1: `standard`
- Method 2: `optimized`
- Method 3: `optimized, vectorized`
- Method 4: `optimized, unmanaged`
- Method 5: `optimized, unmanaged, vectorized`
- Method 6: `optimized, unmanaged, onebit`
- Method 7: `optimized, unmanaged, vectorized, onebit`

Vectorized uses SIMD code and vectorization (currently only properly implemented for x86, x64); NB: many other languages use SIMD for zeroing memory when mine doesn't unless it's specified or it's using an array (if .NET version uses SIMD to allocate arrays, which I don't think .NET 6 does)
Unmanaged uses pointers to allocate rather than arrays.
Onebit uses 1 bit storage.
Code in the optimized methods are very carefully optimized and can be sensitive to certain changes.

# Command-line arguments:
`<program> [runall]`
`<program> runallchecks`
More will be added in the future.

# License:
This is released under [BSD-3](https://opensource.org/licenses/BSD-3-Clause) with "Copyright © 2021 hamarb123". If you want to contribute to this specific implementation, please give copyright to hamarb123 and ask hamarb123.

# TODO:
- Use more SIMD, especially on arm.
- Improve performance of `optimized, unmanaged, vectorized, onebit` because it should be the fastest.
- Add more command-line options
- Use AOT
