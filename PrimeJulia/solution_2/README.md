# Julia solution by @epithet

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Julia can be downloaded from https://julialang.org/downloads/.

This is how you run the code from the command line:
```
$ julia Primes.jl
2, 3, 5, 7, 11, …, 999953, 999959, 999961, 999979, 999983
Passes: 6581, Time: 5.000025987625122, Avg: 0.0007597669028453308, Limit: 1000000, Count: 78498
epithet;6581;5.000025987625122;1;algorithm=base,faithful=yes,bits=1
```

You can also pass the limit as an argument:
```
$ julia Primes.jl 30
2, 3, 5, 7, 11, 13, 17, 19, 23, 29
Passes: 6913442, Time: 5.0, Avg: 7.2322874770628e-7, Limit: 30, Count: 10
epithet;6913442;5.0;1;algorithm=base,faithful=yes,bits=1
```

There are unit tests to verify the implementation (this can take a minute):
```
$ time julia test.jl 
Test Summary:         | Pass  Total
Sieve of Eratosthenes |   56     56

real	0m35.578s
user	0m35.615s
sys	0m0.627s
```

You can also do some benchmarking from the REPL:
```
julia> include("Primes.jl")

julia> @time Primes(1_000_000)
  0.000741 seconds (3 allocations: 61.172 KiB)
Primes(1000000)

julia> using Pkg; Pkg.add("BenchmarkTools")

julia> using BenchmarkTools; @benchmark Primes(1_000_000)
BenchmarkTools.Trial: 
  memory estimate:  61.17 KiB
  allocs estimate:  3
  --------------
  minimum time:     711.094 μs (0.00% GC)
  median time:      745.860 μs (0.00% GC)
  mean time:        748.621 μs (0.06% GC)
  maximum time:     1.482 ms (0.00% GC)
  --------------
  samples:          6662
  evals/sample:     1
```
