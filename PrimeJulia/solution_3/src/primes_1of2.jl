# Solution based on PrimeC/solution_2/sieve_1of2.c by Daniel Spångberg
# 
# While UInts are used as much as possible in the main code for
# consistency, speed reasons, and to mimic sieve_1of2.c, most of the
# defined functions accept any subtype of Integer so it's easier to
# test them in the REPL, and also if we ever need to change `UInt` to
# another type like `Int` for benchmarking purposes.
# Julia compiles specialized code for each specific type anyway, so
# we shouldn't see any significant slowdowns by doing this.
# If you want to use purely UInts, you can simply replace all function
# signatures containing `::Integer` to `::UInt` as well as patch the
# count_primes and get_found_primes functions to use UInts.

module Primes1of2
export PrimeSieve1of2,
       unsafe_find_next_factor_index,
       unsafe_clear_factors!,
       run_sieve!,
       count_primes,
       get_found_primes

import ..AbstractPrimeSieve

const MainUInt = UInt32
const _uint_bit_length = sizeof(MainUInt) * 8
const _div_uint_size_shift = Int(log2(_uint_bit_length))

# Functions like the ones defined below are also used in Julia's Base
# library to speed up things such as Julia's native BitArray type.
@inline _mul2(i::Integer) = i << 1
@inline _div2(i::Integer) = i >> 1
# Map factor to index (3 => 1, 5 => 2, 7 => 3, ...) and vice versa.
@inline _map_to_index(i::Integer) = _div2(i - 1)
@inline _map_to_factor(i::Integer) = _mul2(i) + 1
# This function also takes inspiration from Base._mod64 (bitarray.jl).
@inline _mod_uint_size(i::Integer) = i & (_uint_bit_length - 1)
@inline _div_uint_size(i::Integer) = i >> _div_uint_size_shift


struct PrimeSieve1of2 <: AbstractPrimeSieve
    sieve_size::UInt
    is_not_prime::Vector{MainUInt}
end

# This is more accurate than _div_uint_size(_div2(i)) + 1
@inline _get_num_uints(i::Integer) = _div_uint_size(
    # We only store odd numbers starting from 3
    _div2(
        # Subtract 2 from i to account for the fact that the first
        # index maps to 3, not 1.
        i - 2 + (2 * _uint_bit_length - 1)
    )
)

function PrimeSieve1of2(sieve_size::UInt)
    return PrimeSieve1of2(sieve_size, zeros(MainUInt, _get_num_uints(sieve_size)))
end

# The main() function uses the UInt constructor; this is mostly useful
# for testing in the REPL.
PrimeSieve1of2(sieve_size::Int) = PrimeSieve1of2(UInt(sieve_size))


@inline function unsafe_find_next_factor_index(arr::Vector{<:Unsigned}, start_index::Integer, max_index::Integer)
    # This loop calculates indices as if they are zero-based then adds
    # 1 when accessing the array since Julia uses 1-based indexing.
    zero_index = start_index
    @inbounds while zero_index < max_index
        if iszero(
            arr[_div_uint_size(zero_index) + 1] & (MainUInt(1) << _mod_uint_size(zero_index))
        )
            return zero_index + 1
        end
        zero_index += 1
    end
    # UNSAFE: you need to check this in the caller or make sure it
    # never happens
    return max_index + 1
end

@inline function unsafe_clear_factors!(arr::Vector{<:Unsigned}, factor_index::Integer, max_index::Integer)
    factor = _map_to_factor(factor_index)
    # This function also uses zero-based indexing calculations similar
    # to unsafe_find_next_factor_index.
    zero_index = _div2(factor * factor) - 1
    @inbounds while zero_index < max_index
        arr[_div_uint_size(zero_index) + 1] |= MainUInt(1) << _mod_uint_size(zero_index)
        zero_index += factor
    end
end

function run_sieve!(sieve::PrimeSieve1of2)
    is_not_prime = sieve.is_not_prime
    sieve_size = sieve.sieve_size
    max_bits_index = _map_to_index(sieve_size)
    max_factor_index = _map_to_index(unsafe_trunc(UInt, sqrt(sieve_size)))
    factor_index = UInt(1) # 1 => 3, 2 => 5, 3 => 7, ...
    while factor_index <= max_factor_index
        unsafe_clear_factors!(is_not_prime, factor_index, max_bits_index)
        factor_index = unsafe_find_next_factor_index(is_not_prime, factor_index, max_factor_index)
    end
    return is_not_prime
end


# These functions aren't optimized, but they aren't being benchmarked,
# so it's fine.

@inline function unsafe_get_bit_at_index(arr::Vector{<:Unsigned}, index::Integer)
    zero_index = index - 1
    return @inbounds arr[_div_uint_size(zero_index) + 1] & (MainUInt(1) << _mod_uint_size(zero_index))
end

function count_primes(sieve::PrimeSieve1of2)
    arr = sieve.is_not_prime
    max_bits_index = _map_to_index(sieve.sieve_size)
    # Since we start clearing factors at 3, we include 2 in the count
    # beforehand.
    count = 1
    for i in 1:max_bits_index
        if iszero(unsafe_get_bit_at_index(arr, i))
            count += 1
        end
    end
    return count
end

function get_found_primes(sieve::PrimeSieve1of2)
    arr = sieve.is_not_prime
    sieve_size = sieve.sieve_size
    max_bits_index = _map_to_index(sieve_size)
    output = [2]
    # Int(sieve_size) may overflow if sieve_size is too large (most
    # likely only a problem for 32-bit systems)
    for (index, number) in zip(1:max_bits_index, 3:2:Int(sieve_size))
        if iszero(unsafe_get_bit_at_index(arr, index))
            push!(output, number)
        end
    end
    return output
end

end
