# Julia solution by louie-github

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)

This solution contains mutiple implementations:

1.  [primes_1of2.jl](primes_1of2.jl)

- This is a port of [PrimeC/solution_2/sieve_1of2.c](../../PrimeC/solution_2/sieve_1of2.c)
  by Daniel Spångberg, with a few small tweaks and Julia-specific
  optimizations. This is a sort-of "low-level" style implementation in
  Julia to get as much as speed as possible out of the language. It is
  _not_ designed to be idiomatic Julia code.

2. [primes_unrolled_modulo.jl](primes_unrolled_modulo.jl)

- This implementation was inspired by [PrimeNim/solution_3](../../PrimeNim/solution_3)
  by GordonBGood. It follows a similar method of setting bits using
  generated bit masks based on modulo patterns, but does not follow the
  original implementation entirely, since I can't yet fully understand
  what's going on in the macro. I simply based this solution off the
  solution's [README.md](../../PrimeNim/solution_3/README.md), where
  it is mentioned that there are "eight culling operations per loop"
  "based on modulo patterns that result from stepping within a (even
  bits) byte array".

This solution has only been tested with Julia 1.6. Lower versions of
Julia may work, but are not guaranteed to.

## Descriptions

### primes_1of2.jl

This is currently the simplest implementation.

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
"inverted" bit array, i.e., bits are set when the number is _not prime_
and bits are unset when the number is _prime_. This simplifies the
set_bit operation slightly (`arr[i] |= mask vs. arr[i] &= ~mask`).

### primes_unrolled_modulo.jl

This implementation is similar to primes_1of2.jl, except that it
pre-generates every possible bit mask for all the starting indices and
skip values using modulo patterns.

It then uses an unrolled loop to set the corresponding bits using the
calculated bit masks. This saves some time since we don't have to do
a modulo and bit shift each time we set a bit.

To illustrate this, you can check the generated `unsafe_clear_factors!`
function Expr object:

```
julia> generate_final_loop_clearing_function() |> Base.remove_linenums!
quote
    function unsafe_clear_factors!(arr::Vector{MainUInt}, start::Integer, skip::Integer, stop::Integer)
        start_modulo = start & 7
        skip_modulo = skip & 7
        index = start
        if stop > skip * 7
            unrolled_stop = stop - skip * 7
            if start_modulo == 0
                if skip_modulo == 0
                    index = _unrolled_clear_bits_start_0_skip_0(arr, start, skip, unrolled_stop)
                elseif skip_modulo == 1
                    index = _unrolled_clear_bits_start_0_skip_1(arr, start, skip, unrolled_stop)
                elseif skip_modulo == 2
                    index = _unrolled_clear_bits_start_0_skip_2(arr, start, skip, unrolled_stop)
                elseif skip_modulo == 3
                    index = _unrolled_clear_bits_start_0_skip_3(arr, start, skip, unrolled_stop)
                elseif skip_modulo == 4
                    index = _unrolled_clear_bits_start_0_skip_4(arr, start, skip, unrolled_stop)
                elseif skip_modulo == 5
                    index = _unrolled_clear_bits_start_0_skip_5(arr, start, skip, unrolled_stop)
                elseif skip_modulo == 6
                    index = _unrolled_clear_bits_start_0_skip_6(arr, start, skip, unrolled_stop)
                elseif skip_modulo == 7
                    index = _unrolled_clear_bits_start_0_skip_7(arr, start, skip, unrolled_stop)
                end
            elseif start_modulo == 1
                if skip_modulo == 0
                    index = _unrolled_clear_bits_start_1_skip_0(arr, start, skip, unrolled_stop)
                ...
```

Looking at an example unrolled bit clearing function, we get:

```
julia> generate_loop_clearing_function(0, 3, get_modulo_pattern(0, 3, UINT_BIT_LENGTH)) |> Base.remove_linenums!
quote
    #= /home/louie/dev/PrimesTemp/PrimeJulia/solution_3/primes_unrolled_modulo.jl:91 =# @inline function _unrolled_clear_bits_start_0_skip_3(arr::Vector{MainUInt}, start::Integer, skip::Integer, unrolled_stop::Integer)
            index = start
            #= /home/louie/dev/PrimesTemp/PrimeJulia/solution_3/primes_unrolled_modulo.jl:95 =# @inbounds while index < unrolled_stop
                    begin
                        arr[(index + skip * 0) >> 3 + 1] |= 1
                        arr[(index + skip * 1) >> 3 + 1] |= 8
                        arr[(index + skip * 2) >> 3 + 1] |= 64
                        arr[(index + skip * 3) >> 3 + 1] |= 2
                        arr[(index + skip * 4) >> 3 + 1] |= 16
                        arr[(index + skip * 5) >> 3 + 1] |= 128
                        arr[(index + skip * 6) >> 3 + 1] |= 4
                        arr[(index + skip * 7) >> 3 + 1] |= 32
                    end
                    index += skip * 8
                end
            return index
        end
end
```

As you can see, the generated functions simply end up creating an
unrolled loop with pre-calculated bit masks based on the start index
and the skip value.

These bit masks are pre-calculated based on every possible modulo value
of both the start index and the skip value. For a bit array using
integers of bit length _N_, we will have to generate _N_ \* _N_
unrolled loop functions (_N_ possible start index offsets with _N_
possible skip value bit mask patterns for each offset).

To decide which unrolled loop function to use, the generated
`unsafe_clear_factors!` function uses a large if-elseif statement
(since Julia does not have a switch-case statement) to calculate
the modulo of the start index and the skip value, and then finds the
corresponding function. This _should_ be optimized into a computed
goto by the compiler, but I'm not entirely sure.

This is why the implementation uses a `Vector{UInt8}` so we only have
to calculate 8 \* 8 or 64 functions. If we used a `Vector{UInt32}`, for
example, we would have to calculate 32 \* 32 or 1024 functions, each of
which takes time for Julia to precompile. Plus, the generated code size
from the the inlined functions would be enormous.

The other parts of running the sieve, such as reading the bits in the
array and finding the next factor index, are mostly unchanged from
[primes_1of2.jl](primes_1of2.jl).

## Suggestions and improvements

If you see any room for improvement in the code or have any
suggestions, don't hesitate to open an issue, pull request (PR),
Discussion, or the like. Don't forget to tag me at `@louie-github` so I
can be notified if my personal input is either wanted or needed.
I'm open to fixing stylistic issues or discussing cosmetic changes to
the code, but for those, it might be best if you open a Discussion
first before opening an issue or pull request.

## Run instructions

### Running locally

First, make sure that you have installed [Julia](https://julialang.org/downloads/)
and have verified that your installation works.

To build and run a single implementation locally, run the following
command, replacing `primes_1of2.jl` with the filename of the
implementation you want to run:

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

### Running all implementations at once locally

If you want to run all implementations at once, run:

```
./run.sh
```

You can also pass in a sieve size and/or duration as additional
arguments. These will be passed through to all implementations.

```
./run.sh 1000000 10
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

This will run all implementations, as it uses [run.sh](run.sh).

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

To validate the results given by an implementation, run the following
command, replacing `primes_1of2.jl` with the implementation you wish
to test:

```
julia validate.jl primes_1of2.jl
```

Note that this may take a few minutes to complete.

If you want to verify that an implementation does not go out-of-bounds
when accessing memory since all of them use the `@inbounds` macro quite
liberally, run either the implementation `.jl` file or validate.jl with
`julia --check-bounds=yes`.

```
julia --check-bounds=yes primes_1of2.jl
julia --check-bounds=yes validate.jl
```

Note that this will make the code run a fair bit slower.

## Output

This is an example output from running the solution on a machine with an
Intel Core i5-9300H CPU and 24 GB of RAM running Ubuntu 20.04 LTS under
WSL 2 on Windows 10 Home:

```
❯ ./run.sh
Settings: sieve_size = 1000000 | duration = 5
Number of trues: 78498
primes_1of2.jl: Passes: 10276 | Elapsed: 5.00010085105896 | Passes per second: 2055.1585470168407 | Average pass duration: 0.00048658046429145193
louie-github_port_1of2;10276;5.00010085105896;1;algorithm=base,faithful=yes,bits=1

Settings: sieve_size = 1000000 | duration = 5
Number of trues: 78498
primes_unrolled_modulo.jl: Passes: 15787 | Elapsed: 5.000248908996582 | Passes per second: 3157.2428267712044 | Average pass duration: 0.0003167320522579706
louie-github_unrolled_modulo;15787;5.000248908996582;1;algorithm=base,faithful=yes,bits=1
```
