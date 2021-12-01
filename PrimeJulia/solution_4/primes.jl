# Sieve of Eratosthenes Benchmark in Julia...

module BenchSoE

using InteractiveUtils # used for viewing native code, etc.

const Prime = UInt64

const RANGE = Prime(1000000)

@enum Techniques bittwiddle stride8 stride8block extreme extremebybrid

const FORTIME = Float64(5)

const CPUL1CACHE = Int(16384) # in bytes

const DENSETHRESHOLD = Int(13)

const primeCounts = Dict( Prime(10) => 4,
                          Prime(100) => 25,
                          Prime(1000) => 168,
                          Prime(10000) => 1229,
                          Prime(100000) => 9592,
                          Prime(500000) => 41538,
                          Prime(1000000) => 78498,
                          Prime(10000000) => 664579,
                          Prime(100000000) => 5761455 )

const EXPECTED = primeCounts[RANGE]

const BITMASK = [ UInt8(1), UInt8(2), UInt8(4), UInt8(8),
                 UInt8(16), UInt8(32), UInt8(64), UInt8(128) ]

macro set_sparse_bits(bytevec, start_bit_index, step, limit_bit_index)
    local bv = :($(esc(bytevec))) # get local scaped values
    local strt = :($(esc(start_bit_index)))
    local adv = :($(esc(step)))
    local lmt = :($(esc(limit_bit_index)))
    function mkbody(n) # = :($v) # represents what we want to do for the case
        ndx0 = @eval ($n & 7)
        stp = @eval ($n >>> 3)
        msk0 = @eval (UInt8(1) << $ndx0)
        msk1 = @eval (UInt8(1) << (($ndx0 + $stp) & 7))
        msk2 = @eval (UInt8(1) << (($ndx0 + 2 * $stp) & 7))
        msk3 = @eval (UInt8(1) << (($ndx0 + 3 * $stp) & 7))
        msk4 = @eval (UInt8(1) << (($ndx0 + 4 * $stp) & 7))
        msk5 = @eval (UInt8(1) << (($ndx0 + 5 * $stp) & 7))
        msk6 = @eval (UInt8(1) << (($ndx0 + 6 * $stp) & 7))
        msk7 = @eval (UInt8(1) << (($ndx0 + 7 * $stp) & 7))
        return quote
            r0 = $strt >>> 3
            r1 = (($strt + $adv) >>> 3) - r0
            r2 = (($strt + 2 * $adv) >>> 3) - r0
            r3 = (($strt + 3 * $adv) >>> 3) - r0
            r4 = (($strt + 4 * $adv) >>> 3) - r0
            r5 = (($strt + 5 * $adv) >>> 3) - r0
            r6 = (($strt + 6 * $adv) >>> 3) - r0
            r7 = (($strt + 7 * $adv) >>> 3) - r0
            c = ($strt >>> 3) + 1
            clmt = length($bv) - r7
            @inbounds(
            while c <= clmt
                $bv[c] |= $msk0
                $bv[c + r1] |= $msk1
                $bv[c + r2] |= $msk2
                $bv[c + r3] |= $msk3
                $bv[c + r4] |= $msk4
                $bv[c + r5] |= $msk5
                $bv[c + r6] |= $msk6
                $bv[c + r7] |= $msk7
                c += $adv
            end)
            $(esc(start_bit_index)) = ((c - 1) << 3) + $ndx0
        end
    end
    function supercase(selv)
        function mkelseif(v, ex)
            bdy = mkbody(v)
            Expr(:elseif, :($selv == $v), bdy, ex)
        end
        rst = foldr(mkelseif, 1:62, init=mkbody(63))
        Expr(:if, :($selv == 0), mkbody(0), rst)
    end
    return quote
        sel = (($adv << 3) | ($strt & 7)) & 63
        $(supercase(:(sel)))
        @inbounds(
        for c in $strt:$adv:$lmt
            $bv[(c >>> 3) + 1] |= BITMASK[(c & 7) + 1]
        end)
    end
end

macro set_dense_bits(bytevec, start_bit_index, step, limit_bit_index)
    local bv = :($(esc(bytevec))) # get local scaped values
    local strt = :($(esc(start_bit_index)))
    local adv = :($(esc(step)))
    local lmt = :($(esc(limit_bit_index)))
    function mkbody(n, wvq)
        stp = @eval ($n + $n + 3)
        function set_word_pattern(wv, wpi)
            function mkset(bi)
                owi = (bi - stp) >> 6; nwi = (bi + stp) >>> 6
                wi = bi >>> 6; msk = UInt64(1) << (bi & 63)
                if owi < wi && wi < nwi
                    :( $wv[$wpi + $wi] |= $msk)
                elseif owi < wi
                    :(v = $wv[$wpi + $wi] | $msk)
                elseif wi < nwi
                    :($wv[$wpi + $wi] |= v | $msk)
                else
                    :(v |= $msk)
                end
            end
            sets = [ mkset(i*stp) for i = 0:63 ]
            return Expr(:block, sets...)
        end
        quote
            wordIndex = ($strt >>> 6) + 1 # for views, we can index words directly
            wordLimit = length(cws) - ($adv - 1)
            @inbounds(
            while wordIndex <= wordLimit
                $(set_word_pattern(wvq, :wordIndex))
                wordIndex += $adv
            end)
            $(esc(start_bit_index)) = (wordIndex - 1) << 6
        end
    end
    function supercase(selv, wrdbuf)
        function mkelseif(v, ex)
            bdy = mkbody(v, wrdbuf)
            Expr(:elseif, :($selv == $v), bdy, ex)
        end
        dtlmt = (DENSETHRESHOLD - 3) >>> 1
        rst = foldr(mkelseif, 1:dtlmt-1, init=mkbody(dtlmt, wrdbuf))
        Expr(:if, :($selv == 0), mkbody(0, wrdbuf), rst)
    end
    quote
        @inbounds(
        for ci in $strt:$adv:$lmt # mark sieve buffer to next even word
            if ci & 63 == 0 
                $(esc(start_bit_index)) = ci # change input start index value
                break
            end
            $bv[(ci >>> 3) + 1] |= BITMASK[(ci & 7) + 1]
        end)
        sel = ($adv - 3) >>> 1
        cws = reinterpret(UInt64, $bv)
        $(supercase(:sel, :cws)) # also changes the input start index value
        for c in $strt:$adv:$lmt # mark for the rest of the sieve buffer
            $bv[(c >>> 3) + 1] |= BITMASK[(c & 7) + 1]
        end
    end
end

struct Primes
    rangep :: Prime
    cmpstsbits :: Vector{UInt8}
    function Primes(rng :: Prime, tec :: Techniques)
        limi :: Int = (rng - 3) >>> 1
        size :: Int = (limi >>> 3) + 1
        cmpsts :: Vector{UInt8} =
          fill(UInt8(0), (size + 7) & (-8)) # round up to UInt64
        strts :: Vector{Int} = fill(0, 8) # used for stride8block
        @inbounds(
        for i in 0:limi
            start = (i + i) * (i + 3) + 3
            if start > limi break end
            if cmpsts[(i >>> 3) + 1] & BITMASK[(i & 7) + 1] != 0 continue end
            bp = i + i + 3

            if tec == bittwiddle
                @inbounds(
                while start <= limi
                    cmpsts[(start >>> 3) + 1] |= BITMASK[(start & 7) + 1]
                    start += bp
                end)

            elseif tec == stride8
                startlmt = min(limi, start + (bp << 3) - 1)
                while start <= startlmt
                    culli = (start >>> 3) + 1; mask = BITMASK[(start & 7) + 1]
                    @inbounds(
                        while culli <= size
                            cmpsts[culli] |= mask; culli += bp
                        end)
                    start += bp
                end

            elseif tec == stride8block
                bp2 = bp + bp
                bp3 = bp + bp2
                bp4 = bp2 + bp2
                bp5 = bp4 + bp
                bp6 = bp4 + bp2
                bp5 = bp3 + bp2
                bp6 = bp3 + bp3
                bp7 = bp4 + bp3
                bp8 = bp4 + bp4
                for _ in 0:7
                    strts[(start & 7) + 1] = (start >>> 3) + 1
                    start += bp
                end
                @inbounds(
                for pgndx in 1:CPUL1CACHE:size
                    pglmt = pgndx + min(CPUL1CACHE - 1, size - pgndx)
                    for si in 1:8
                        mask = BITMASK[si]            
                        c = strts[si]
                        while c <= (pglmt - bp7)
                            cmpsts[c] |= mask
                            cmpsts[c + bp] |= mask
                            cmpsts[c + bp2] |= mask
                            cmpsts[c + bp3] |= mask
                            cmpsts[c + bp4] |= mask
                            cmpsts[c + bp5] |= mask
                            cmpsts[c + bp6] |= mask
                            cmpsts[c + bp7] |= mask
                            c += bp8
                        end
                        while c <= pglmt
                            cmpsts[c] |= mask
                            c += bp
                        end
                        strts[si] = c
                    end
                end)

            elseif tec == extreme
                @set_sparse_bits(cmpsts, start, bp, limi)
            
            else # is extremehybrid
                if bp < DENSETHRESHOLD
                    @set_dense_bits(cmpsts, start, bp, limi)
                else
                    @set_sparse_bits(cmpsts, start, bp, limi)
                end

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
        while i < lmt && cs[(i >>> 3) + 1] & BITMASK[(i & 7) + 1] != 0
            i += 1 end)
        if i >= lmt return nothing end
        return (i + i + 3, i + 2)
    end
end

function bench(tec :: Techniques)
    passes = 0
    starttime = time()
    while true
        primes = Primes(Prime(RANGE), tec)
        elapsed = time() - starttime
        if elapsed >= FORTIME break end
    end
    starttime = time()
    while true
        primes = Primes(Prime(RANGE), tec)
        passes += 1
        elapsed = time() - starttime
        if elapsed >= FORTIME
            count = length(primes)
            if count == EXPECTED
                label = "GordonBGood_"
                if tec == bittwiddle label *= "bittwiddle"
                elseif tec == stride8 label *= "stride8"
                elseif tec == stride8block label *= "stride8block16k"
                elseif tec == extreme label *= "extreme"
                else label *= "extremehybrid"
                end
                println("$label;$passes;$elapsed;1;algorithm=base,faithful=yes,bits=1")
            else
                println("Invalid result:  ", count, " passes:  ", passes)
            end
            break
        end
    end
end

#= # add a space between the `#` and the `=` to uncomment
# allows to view the expanded macros when compiled...
rng = (1000000 - 3) >>> 1
tstbv = fill(UInt8(0), ((rng + 64) >>> 3) & (-8))
start = 3
bp = 3
lmt = 499998
println(@macroexpand @set_sparse_bits(tstbv, start, bp, lmt))
println(@macroexpand @set_dense_bits(tstbv, start, bp, lmt))
# =#

for t in instances(Techniques) bench(t) end

end # module

