include("primes_1of2.jl")

# Taken from PrimeCPP/solution_1/PrimeCPP.cpp
const resultsDictionary = Dict(
    10 => 4,
    100 => 25,
    1000 => 168,
    10000 => 1229,
    100000 => 9592,
    1000000 => 78498,
    10000000 => 664579,
    100000000 => 5761455,
    1000000000 => 50847534
)

# UInt16 can't even handle 1 million
if UInt == UInt16
    error("UInt is detected as UInt16. Cannot run tests.")
# 32-bit systems cannot represent 10^10 or 10_000_000_000
elseif UInt == UInt32
    @warn "Skipping test 10000000000 => 455052511 since machine is 32-bit."
else
    push!(resultsDictionary, 10000000000 => 455052511)
end

function validate_results()
    output = Bool[]
    for (sieve_size, expected_result) in resultsDictionary
        print("$sieve_size => $expected_result: ")
        s = PrimeSieve(sieve_size)
        run_sieve!(s)
        result = count_primes(s) == expected_result
        println(result)
        push!(output, result)
    end
    return all(output)
end

if abspath(PROGRAM_FILE) == @__FILE__
    const is_valid = validate_results()
    println("All tests return true: $is_valid")
end