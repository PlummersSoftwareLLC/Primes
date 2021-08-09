include("primes_1of2.jl") ; using .Primes1of2

const IMPLEMENTATIONS = [
    PrimeSieve1of2
]
const IMPLEMENTATION_NAMES = Set(string(i) for i in IMPLEMENTATIONS)
# We use this instead of defining an abstract supertype since we want
# each module to be as self-contained as possible. This is probably not
# very clean nor extensible, so this might need to be changed sometime
# in the future.
AbstractPrimeSieve = Union{IMPLEMENTATIONS...}

function main_benchmark(
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
        main_benchmark(implementation, sieve_size, duration)
        println(stderr)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end