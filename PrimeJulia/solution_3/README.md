# Julia solution by louie-github
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)

This solution is a port of [PrimeC/solution_2/sieve_1of2.c](../../PrimeC/solution_2/sieve_1of2.c)
by Daniel Spångberg, with a few small tweaks and Julia-specific
optimizations. This is a sort-of "low-level" style implementation in
Julia to get as much as speed as possible out of the language. It is
*not* designed to be idiomatic Julia code.

This solution requires at least **Julia 1.5** to run. Julia 1.6 is
recommended and is used in the Docker image.

## Description

Instead of using Julia's native `BitVector` type, this solution
manually implements a bit array using a `Vector{UInt32}` since 32-bit
integers seem to give the best performance. You can switch which
unsigned integer type you want to use by changing the line:
```
const MainUInt = UInt32
```
Change `UInt32` to whichever unsigned integer type you wish to use.

Manual bit shifting and bit masking is employed to both read and set
bits in the bit array. Bitwise operations are used as much as possible
over arithmetic operations since they are much faster and simpler to
execute.

For readability, these bitwise operations are wrapped in functions such
as `_div2` and `_mod_uint_size`. This is not unlike what Julia itself
does with its native `BitVector` type (see
[bitarray.jl](https://github.com/JuliaLang/julia/blob/master/base/bitarray.jl)).

This implementation only stores bits for the odd numbers above 3 in an
"inverted" bit array, i.e., bits are set when the number is *not prime*
and bits are unset when the number is *prime*. This simplifies the
set_bit operation slightly (`arr[i] |= mask vs. arr[i] &= ~mask`).

If you see any room for improvement in the code or have any
suggestions, don't hesitate to open an issue, pull request (PR), 
Discussion, or the like. Don't forget to tag me at `@louie-github` so I
can be notified if my personal input is either wanted or needed.
I'm open to fixing stylistic issues or discussing cosmetic changes to
the code, but for those, it might be best if you open a Discussion
first before opening an issue or pull request.


## Run instructions

### Running locally

First, make sure that you have installed
[Julia 1.5 or newer](https://julialang.org/downloads/) and have
verified that your installation works.

To build and run the solution locally, run the following command:
```
julia primes_1of2.jl
```

Optionally, you can specify a sieve size as an additional command line argument:
```
julia primes_1of2.jl 1000
```

You can also specify the duration to run the benchmark for after the
sieve size:
```
julia primes_1of2.jl 1000000 10
```

### Running in Docker

If you want to run this solution in a Docker container, follow the steps below.

1. Build or update the Docker image. You can skip this step if you have
   already built the `primejulia-3` image.

```
docker build --pull -t primejulia-3 .
```

2. Run the `primejulia-3` image as a container.
```
docker run -it --rm primejulia-3
```
You can also pass in a sieve size and/or a duration as additional
arguments similar to when you run the solution locally:
```
docker run -it --rm primejulia-3 1000
docker run -it --rm primejulia-3 1000000 10
```

3. If you want to remove the built Docker image, run:
```
docker image rm primejulia-3
```

This solution's [Dockerfile](Dockerfile) uses the `julia:1.6-buster`
image to provide maximum support for different architectures, most
importantly ARM64 and x86_64. This also helps avoid any possible issues
caused by Julia's [Tier 3 support for musl](https://julialang.org/downloads/#currently_supported_platforms),
including Alpine Linux.


## Validating results

The validation code has been moved to a separate file:
[validate.jl](validate.jl).

Since 32-bit systems cannot represent the final element of the results
dictionary (10^10 or 10000000000) in a single `UInt32`, this number is
skipped when running validate.jl on 32 bit systems.

To validate the results given by [primes_1of2.jl](primes_1of2.jl), run:
```
julia validate.jl
```
Note that this may take a few minutes to complete.

If you want to verify that primes_1of2.jl does not go out-of-bounds
when accessing memory since it uses `@inbounds` quite liberally, run
either primes_1of2.jl or validate.jl with `julia --check-bounds=yes`.
```
julia --check-bounds=yes primes_1of2.jl
julia --check-bounds=yes validate.jl
```
Note that this will make the code run a fair bit slower.


## Output

This is the output from running the solution on a machine with an Intel
Core i5-9300H CPU and 24 GB of RAM running Windows 10 Home:
```
PS D:\Office Files\Programming\Primes> julia primes_1of2.jl
Settings: sieve_size = 1000000 | duration = 5
Number of trues: 78498
primes_1of2.jl: Passes: 9146 | Elapsed: 5.0 | Passes per second: 1829.2 | Average pass duration: 0.0005466870763175158
louie-github_port_1of2;9146;5.0;1;algorithm=base,faithful=yes,bits=1
```
On said machine, when no heavy tasks are running, the number of passes
usually ranges between 8900 and 9300.
