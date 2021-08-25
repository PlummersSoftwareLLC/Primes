# Sieve of Eratosthenes Benchmark in Julia...

module BenchSoE

const Prime = UInt64

const RANGE = Prime(1000000)

const FORTIME = 5

const CPUL1CACHE = 16384 # in bytes

const primeCounts = Dict( Prime(10) => 4,
                          Prime(100) => 25,
                          Prime(1000) => 168,
                          Prime(10000) => 1229,
                          Prime(100000) => 9592,
                          Prime(1000000) => 78498,
                          Prime(10000000) => 664579,
                          Prime(100000000) => 5761455 )

const EXPECTED = primeCounts[RANGE]

const BITMSK = [ UInt8(1), UInt8(2), UInt8(4), UInt8(8),
                 UInt8(16), UInt8(32), UInt8(64), UInt8(128) ]

struct Primes
    rangep :: Prime
    cmpstsbits :: Vector{UInt8}
    function Primes(rng :: Prime)
        limi :: Int = (rng - 3) >>> 1
        size :: Int = (limi >>> 3) + 1
        cmpsts :: Vector{UInt8} =
          fill(UInt8(0), (size + 7) & (-8)) # round up to UInt64
        strts :: Vector{Int} = fill(0, 8)
        @inbounds(
        for i in 0:limi
            start = (i + i) * (i + 3) + 3
            if start > limi break end
            if cmpsts[(i >>> 3) + 1] & BITMSK[(i & 7) + 1] == 0
                bp = i + i + 3
                bp2 = bp << 1
                bp3 = bp + bp2
                bp4 = bp << 2
                for x in 0:7
                    ns = start + x * bp
                    strts[(ns & 7) + 1] = (ns >>> 3) + 1
                end
                @inbounds(
                for pgndx in 1:CPUL1CACHE:size
                    pglmt = pgndx + min(CPUL1CACHE - 1, size - pgndx)
                    for si in 1:8
                        mask = BITMSK[si]            
                        c = strts[si]
                        while c <= (pglmt - bp3)
                            cmpsts[c] |= mask
                            cmpsts[c + bp] |= mask
                            cmpsts[c + bp2] |= mask
                            cmpsts[c + bp3] |= mask
                            c += bp4
                        end
                        while c <= pglmt
                            cmpsts[c] |= mask
                            c += bp
                        end
                        strts[si] = c
                    end
                end)
            end
        end)
        return new(rng, cmpsts)
    end
end
 
Base.eltype(::Type{Primes}) = Prime
 
function Base.length(P::Primes)::Int64
    if P.rangep < 3
        if P.rangep < 2 return 0 else return 1 end
    end
    cs = reinterpret(UInt64, P.cmpstsbits)
    clmt = length(cs) - 1
    cnt = 1 + clmt * 64 + 64
    for i in 1:clmt cnt -= count_ones(cs[i]) end
    mask = (UInt64(0) - UInt64(2)) << (((P.rangep - 3) >>> 1) & 63)
    cnt -= count_ones(cs[clmt + 1] | mask)
    return cnt
end
 
function Base.iterate(P::Primes, state::Int = 0)::
                                        Union{Tuple{Prime, Int64}, Nothing}
    
    if P.rangep < 2 return nothing end
    lmt = Int((P.rangep - 1)) ÷ 2
    if state > lmt return nothing end
    if state <= 0 return (UInt64(2), 1) end
    let
        cs = P.cmpstsbits
        i = state - 1
        @inbounds(
        while i < lmt && cs[(i >>> 3) + 1] & BITMSK[(i & 7) + 1] != 0
            i += 1 end)
        if i >= lmt return nothing end
        return (i + i + 3, i + 2)
    end
end

function bench()
#    println(@time length(Primes(Prime(100)))) # warm up JIT
    passes = 0
    starttime = time()
    while true
        primes = Primes(Prime(RANGE))
        passes += 1
        elapsed = time() - starttime
        if elapsed >= FORTIME
            count = length(primes)
            if count == EXPECTED
                println("GordonBGood_unpeeled;$passes;$elapsed;1;algorithm=base,faithful=yes,bits=1")
            else
                println("Invalid result:  ", count, " passes:  ", passes)
            end
            break
        end
    end
end

bench()

end # module

