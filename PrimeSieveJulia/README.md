# PrimeSieveJulia
PrimeSieveJulia is a Julia implementation of the code from Dave Plummer's Primes repository which imitates Dave's prime-sieving algorithm.

## Performance
`Passes: 5688, Time: 10.0, Avg: 0.0017580872011251757, Limit: 1000000, Count: 78498, Valid: true`

## How to run
Install Julia version 1.5.4 or higher. Refer to the download and install instructions at (https://julialang.org/downloads/). To run from a Windows console, locate the julia.exe file and the location of this code's source file, then input:

`C:\> julia.exe PrimeSieveJulia.jl`

To run from the Julia REPL, locate this code's source file and input

`julia> include("PrimeSieveJulia.jl")`
