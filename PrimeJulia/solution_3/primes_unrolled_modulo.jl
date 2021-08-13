# Solution based on PrimeNim/solution_3 by GordonBGood.

# This implementation uses a Vector{UInt8} instead of UInt32 since
# using 32-bit integers would mean needing to generate 32 * 32 (1024)
# different functions for all possible start index offsets and skip
# modulo pattern bit masks, so instead we settle for 8 * 8 (64).
const MainUInt = UInt8
const UINT_BIT_LENGTH = sizeof(MainUInt) * 8
const UINT_BIT_SHIFT = Int(log2(UINT_BIT_LENGTH))

@inline _mul2(i::Integer) = i << 1
@inline _div2(i::Integer) = i >> 1
# Map factor to index (3 => 1, 5 => 2, 7 => 3, ...) and vice versa.
@inline _map_to_index(i::Integer) = _div2(i - 1)
@inline _map_to_factor(i::Integer) = _mul2(i) + 1
# This function also takes inspiration from Base._mod64 (bitarray.jl).
@inline _mod_uint_length(i::Integer) = i & (UINT_BIT_LENGTH - 1)
@inline _div_uint_length(i::Integer) = i >> UINT_BIT_SHIFT


struct PrimeSieve
    sieve_size::UInt
    is_not_prime::Vector{MainUInt}
end

# This is more accurate than _div_uint_size(_div2(i)) + 1
@inline _get_num_uints(i::Integer) = _div_uint_length(
    # We only store odd numbers starting from 3.
    _div2(i + (2 * UINT_BIT_LENGTH - 1))
)

function PrimeSieve(sieve_size::UInt)
    return PrimeSieve(sieve_size, zeros(MainUInt, _get_num_uints(sieve_size)))
end

# The main() function uses the UInt constructor; this is mostly useful
# for testing in the REPL.
PrimeSieve(sieve_size::Int) = PrimeSieve(UInt(sieve_size))

"""
```
get_modulo_pattern([start_index::Integer], skip::Integer, bit_length::Integer)
```

This function calculates the modulo patterns used for the factor
clearing loop. It takes into account both the start index and the skip.
The modulo patterns of the skip are calculated first, and then shifted
by the initial offset of the start index.

This function should return a `Vector` of length `bit_length`, even if
the corresponding modulo pattern is shorter than that.
"""

function get_modulo_pattern(start::Integer, skip::Integer, bit_length::Integer)
    @assert (skip >= 0) "skip must not be less than 0."
    modulo_increment = skip % bit_length
    output = Vector{typeof(modulo_increment)}(undef, bit_length)
    current_modulo = start % bit_length
    for index in eachindex(output)
        output[index] = current_modulo
        current_modulo += modulo_increment
        current_modulo %= bit_length
    end
    return output
end

function generate_modulo_patterns(bit_length::T) where T <: Integer
    output = Dict{Tuple{T,T},Vector{T}}()
    for start in 0:bit_length - 1
        for skip in 0:bit_length - 1
            output[(start, skip)] = get_modulo_pattern(start, skip, bit_length)
        end
    end
    return output
end

function generate_loop_clearing_function(
    start::Integer, skip::Integer, modulo_pattern::AbstractVector{T}
) where T <: Integer
    func_name = Symbol("_unrolled_clear_bits_start_", start, "_skip_", skip)
    body_expr = Expr(:block)
    body_A = body_expr.args
    for (skip_iter, bit_index) in enumerate(modulo_pattern)
        push!(body_A, :(
            arr[(index + skip * $(skip_iter - 1)) >> $UINT_BIT_SHIFT + 1] |=
            $(MainUInt(1) << bit_index)
        ))
    end
    return :(
        function $func_name(
            arr::Vector{MainUInt}, start::Integer, skip::Integer, unrolled_stop::Integer
        )
        index = start
        @inbounds while index < unrolled_stop
            $body_expr
            index += skip * $(length(modulo_pattern))
        end
    end
    )
end

function generate_loop_clearing_function(start::Integer, skip::Integer)
    return generate_loop_clearing_function(
        start, skip, get_modulo_pattern(start, skip, UINT_BIT_LENGTH)
    )
end

# This is really hacky; we are evaluating function definitions in
# global scope. It's better than manually creating a 700 or so line
# function, though, I guess.
for ((start, skip), modulo_pattern) in generate_modulo_patterns(UINT_BIT_LENGTH)
    eval(generate_loop_clearing_function(start, skip, modulo_pattern))
end

@inline function unsafe_clear_factors!(arr::Vector{<:Unsigned}, factor_index::Integer, max_index::Integer)
    factor = _map_to_factor(factor_index)
    # This function also uses zero-based indexing calculations similar
    # to unsafe_find_next_factor_index.
    zero_index = _div2(factor * factor)
    @inbounds while zero_index < max_index
        arr[_div_uint_length(zero_index) + 1] |= MainUInt(1) << _mod_uint_length(zero_index)
        zero_index += factor
    end
end

function run_sieve!(sieve::PrimeSieve)
    is_not_prime = sieve.is_not_prime
    sieve_size = sieve.sieve_size
    max_bits_index = _map_to_index(sieve_size)
    max_factor_index = _map_to_index(unsafe_trunc(UInt, sqrt(sieve_size)))
    factor_index = UInt(1) # 0 => 1, 1 => 3, 1 => 5, 2 => 7, ...
    @inbounds while factor_index <= max_factor_index
        if iszero(
            is_not_prime[_div_uint_length(factor_index) + 1] &
            (MainUInt(1) << _mod_uint_length(factor_index))
        )
            unsafe_clear_factors!(is_not_prime, factor_index, max_bits_index)
        end
        factor_index += 1
    end
    return is_not_prime
end


# These functions aren't optimized, but they aren't being benchmarked,
# so it's fine.

@inline function unsafe_get_bit_at_index(arr::Vector{<:Unsigned}, index::Integer)
    zero_index = index - 1
    return @inbounds(
        arr[_div_uint_length(zero_index) + 1] & (MainUInt(1) << _mod_uint_length(zero_index))
    )
end

function count_primes(sieve::PrimeSieve)
    arr = sieve.is_not_prime
    max_bits_index = _map_to_index(sieve.sieve_size)
    # Since we start clearing factors at 3, we include 2 in the count
    # beforehand.
    count = 1
    for i in 2:max_bits_index
        if iszero(unsafe_get_bit_at_index(arr, i))
            count += 1
        end
    end
    return count
end

function get_found_primes(sieve::PrimeSieve)
    arr = sieve.is_not_prime
    sieve_size = sieve.sieve_size
    max_bits_index = _map_to_index(sieve_size)
    output = [2]
    # Int(sieve_size) may overflow if sieve_size is too large (most
    # likely only a problem for 32-bit systems)
    for (index, number) in zip(2:max_bits_index, 3:2:Int(sieve_size))
        if iszero(unsafe_get_bit_at_index(arr, index))
            push!(output, number)
        end
    end
    return output
end

function main_benchmark(sieve_size::Integer, duration::Integer)
    println(stderr, "Settings: sieve_size = $sieve_size | duration = $duration")
    start_time = time()
    elapsed = 0
    passes = 0
    # Ensure precompilation before we run the main benchmark.
    sieve_instance = PrimeSieve(sieve_size)
    run_sieve!(sieve_instance)
    while elapsed < duration
        run_sieve!(PrimeSieve(sieve_size))
        passes += 1
        elapsed = time() - start_time
    end
    println(stderr, "Number of trues: ", count_primes(sieve_instance))
    println(
        stderr,
        "primes_unrolled_modulo.jl: ",
        join(
            [
                "Passes: $passes",
                "Elapsed: $elapsed",
                "Passes per second: $(passes / elapsed)",
                "Average pass duration: $(elapsed / passes)",
            ],
            " | ",
        )
    )
    println("louie-github_unrolled_modulo;$passes;$elapsed;1;algorithm=base,faithful=yes,bits=1")
end

function main(args::Vector{String}=ARGS)
    args_sieve_size = length(args) >= 1 ? tryparse(Int, args[1]) : nothing
    args_duration = length(args) >= 2 ? tryparse(Int, args[2]) : nothing
    # Techincally, we could just keep sieve_size as an Int since we
    # have an Int constructor for PrimeSieve, but let's just use UInt
    # to be consistent.
    sieve_size = isnothing(args_sieve_size) ? UInt(1_000_000) : UInt(args_sieve_size)
    duration = isnothing(args_duration) ? 5 : args_duration
    main_benchmark(sieve_size, duration)
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
