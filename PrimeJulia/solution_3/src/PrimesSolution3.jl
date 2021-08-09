module PrimesSolution3

# Exported functions from the individual implementations
export unsafe_find_next_factor_index,
       unsafe_clear_factors!,
       run_sieve!,
       count_primes,
       get_found_primes,
# Main exported names from module PrimesSolution3
       AbstractPrimeSieve,
       IMPLEMENTATIONS,
       IMPLEMENTATION_NAMES,
       benchmark_implementation


abstract type AbstractPrimeSieve end

include("primes_1of2.jl")
using .Primes1of2

const IMPLEMENTATIONS = [
    PrimeSieve1of2
]
const IMPLEMENTATION_NAMES = Set(string(i) for i in IMPLEMENTATIONS)

function benchmark_implementation(
    PrimeSieveImplementation::Type{<:AbstractPrimeSieve},
    sieve_size::Integer,
    duration::Integer
)
    start_time = time()
    elapsed = 0
    passes = 0
    # Ensure precompilation before we run the main benchmark.
    sieve_instance = PrimeSieveImplementation(sieve_size)
    run_sieve!(sieve_instance)
    while elapsed < duration
        run_sieve!(PrimeSieveImplementation(sieve_size))
        passes += 1
        elapsed = time() - start_time
    end
    println(stderr, "Number of trues: ", count_primes(sieve_instance))
    println(
        stderr,
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
    # TODO: Allow implementations to set algorithm, faithfulness, and
    # bits tags.
    println(
        "louie-github_" * string(nameof(PrimeSieveImplementation)) *
        ";$passes;$elapsed;1;algorithm=base,faithful=yes,bits=1"
    )
end

function main(args::Vector{String}=ARGS)
    args_sieve_size = length(args) >= 1 ? tryparse(Int, args[1]) : nothing
    args_duration = length(args) >= 2 ? tryparse(Int, args[2]) : nothing
    # Technically, we could just keep sieve_size as an Int since we
    # have an Int constructor for PrimeSieve, but let's just use UInt
    # to be consistent.
    sieve_size = isnothing(args_sieve_size) ? UInt(1_000_000) : UInt(args_sieve_size)
    duration = isnothing(args_duration) ? 5 : args_duration
    println(stderr, "Settings: sieve_size = $sieve_size | duration = $duration")
    for implementation in IMPLEMENTATIONS
        println(stderr, "Benchmarking implementation: $(nameof(implementation))")
        benchmark_implementation(implementation, sieve_size, duration)
        println(stderr)
    end
end

end
