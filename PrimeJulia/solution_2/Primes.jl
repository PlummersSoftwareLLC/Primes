struct Primes
    data::BitVector
    function Primes(limit::Int)
        limit ≤ 1 && (limit = 0)
        sieve = trues(div(1+limit, 2))
        i = 2

        while true
            factor = 2i-1
            range = div(1+factor^2, 2):factor:length(sieve)
            isempty(range) && break
            sieve[range] .= false

            i = findnext(sieve, i+1)
        end

        new(sieve)
    end
end

# implement `show` to get a nicer string representation e.g. in the REPL
Base.show(io::IO, primes::Primes) = print(io, "Primes($(2length(primes.data)))")

# implement `eltype`, `length`, and `iterate` so that we can iterate over and collect into vectors
Base.eltype(primes::Primes) = Int
Base.length(primes::Primes) = sum(primes.data)
Base.iterate(primes::Primes) = get(primes.data, 1, false) ? (2, 2) : nothing
function Base.iterate(primes::Primes, i)
    i = findnext(primes.data, i)
    i != nothing ? (2i-1, i+1) : nothing
end
function Base.iterate(r::Iterators.Reverse{Primes}, i=length(r.itr.data))
    i == 0 && return nothing
    i == 1 && return (2, 0)
    i = findprev(r.itr.data, i)
    (2i-1, i-1)
end

function main()
    limit = something(map(s -> tryparse(Int, s), ARGS)..., 1_000_000)

    passes = 0
    start = time()
    local primes
    while (duration = time()-start) < 5
        primes = Primes(limit)
        passes += 1
    end

    println(stderr, join(length(primes) > 10 ? [first(primes, 5)..., '…', last(primes, 5)...] : primes, ", "))
    println(stderr, "Passes: $passes, Time: $duration, Avg: $(duration/passes), Limit: $limit, Count: $(length(primes))")
    println("epithet-jl;$passes;$duration;1;algorithm=base,faithful=yes,bits=1")
end

# the following block executes only if this file is the main script
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
