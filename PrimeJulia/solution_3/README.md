# Julia solution by louie-github

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)

This solution is a collection of implementations separated into their
own files under the [src](src) directory:

1.  Primes1of2: [primes_1of2.jl](src/primes_1of2.jl), a port of [PrimeC/solution_2/sieve_1of2.c](../../PrimeC/solution_2/sieve_1of2.c) by Daniel Spångberg

    This port also contains a few small tweaks and Julia-specific
    optimizations. This is a sort-of "low-level" style implementation
    in Julia to get as much as speed as possible out of the language. It is
    _not_ designed to be idiomatic Julia code.

This solution has only been tested on **Julia 1.6**. Older versions may
work, but your mileage may vary.

### primes_1of2.jl

Instead of using Julia's native `BitVector` type, this solution
manually implements a bit array using a `Vector{UInt32}` since 32-bit
integers seem to give the best performance. You can switch which
unsigned integer type you want to use by changing the line:

```
const MainUInt = UInt32
```

Change `UInt32` to whatever unsigned integer type you wish to use.

Manual bit shifting and bit masking is employed to both read and set
bits in the bit array. Bitwise operations are used as much as possible
over arithmetic operations since they are much faster and simpler to
execute.

For readability, these bitwise operations are wrapped in functions such
as `_div2` and `_mod_uint_size`. This is not unlike what Julia itself
does with its native `BitVector` type (see
[bitarray.jl](https://github.com/JuliaLang/julia/blob/master/base/bitarray.jl)).

This implementation only stores bits for the odd numbers above 3 in an
"inverted" bit array, i.e., bits are set when the number is _not prime_
and bits are unset when the number is _prime_. This simplifies the
set_bit operation slightly (`arr[i] |= mask vs. arr[i] &= ~mask`).

## Suggestions

If you see any room for improvement in the code or have any
suggestions, don't hesitate to open an issue, pull request (PR),
Discussion, or the like. Don't forget to tag me at `@louie-github` so I
can be notified if my personal input is either wanted or needed.
I'm open to fixing stylistic issues or discussing cosmetic changes to
the code, but for those, it might be best if you open a Discussion
first before opening an issue or pull request.

## Run instructions

### Running locally

First, make sure that you have [installed Julia](https://julialang.org/downloads/),
preferably **Julia 1.6** or newer, and have verified that your
installation works.

To build and run the solution locally, run the following command:

```
julia --project=. main.jl
```

Optionally, you can specify a sieve size as an additional command line argument:

```
julia --project=. main.jl 1000
```

You can also specify the duration to run the benchmark for after the
sieve size:

```
julia --project=. main.jl 1000000 10
```

TODO: Allow running a specific implementation instead of all of them at
once.

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

To validate the results given by the implementations, run:

```
julia --project=. validate.jl
```

Note that this may take a few minutes to complete.

If you want to verify that the implementations do not go out-of-bounds
when accessing memory since they use `@inbounds` quite liberally, run
either main.jl or validate.jl with `julia --check-bounds=yes`.

```
julia --project=. --check-bounds=yes main.jl
julia --project=. --check-bounds=yes validate.jl
```

Note that this will make the code run a fair bit slower.

## Output

This is the output from running the solution on a machine with an Intel
Core i5-9300H CPU and 24 GB of RAM running Ubuntu 20.04 LTS under WSL2
under Windows 10 Home:

```
❯ julia --project=. main.jl
Settings: sieve_size = 1000000 | duration = 5
Benchmarking implementation: PrimeSieve1of2
Number of trues: 78498
Passes: 10273 | Elapsed: 5.000046968460083 | Passes per second: 2054.580699901682 | Average pass duration: 0.0004867173141691894
louie-github_PrimeSieve1of2;10273;5.000046968460083;1;algorithm=base,faithful=yes,bits=1
```

On said machine, when no heavy tasks are running, the number of passes
usually ranges between 9990 and 10350.

### Output (Raspberry Pi)

On the other hand, this is the output when running the solution on a
Raspberry Pi 3 Model B Rev 1.2 under Docker running on Raspberry Pi OS
(buster, 32-bit):

```
❯ docker run -it --rm primejulia-3
Settings: sieve_size = 1000000 | duration = 5
Benchmarking implementation: PrimeSieve1of2
Number of trues: 78498
Passes: 1153 | Elapsed: 5.003839015960693 | Passes per second: 230.42308042330856 | Average pass duration: 0.004339843032056109
louie-github_PrimeSieve1of2;1153;5.003839015960693;1;algorithm=base,faithful=yes,bits=1
```
