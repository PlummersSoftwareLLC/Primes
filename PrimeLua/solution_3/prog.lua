--[[

    Luajit Solution 3
    by Mooshua, donated to the public domain (CC0)

]]

local ARGS = {...}
local ffi = require("ffi")
local clock = os.clock
local __mi, __ma = math.min, math.max
local band, bor, lshift, rshift = bit.band, bit.bor, bit.lshift, bit.rshift
local PRIMES = { 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997}
local SIZE = 1000000

local function CalcMax(Unroll)
    return 997*2*Unroll
end

local function Results(Name,Iterations,Time, Size)
    return string.format("%s;%s;%s;1;algorithm=wheel,faithful=false,storage=%s,parallelism=1", Name, Iterations, Time, Size)
end

local function Compile(Time, Unroll)

    Time = Time or 5
    Unroll = Unroll or 8
    
    local Inner = ""
    for i = 1, Unroll do
        Inner = Inner .. "\n\t ARENA[ x" .. string.rep(" + vk", i) .." ] = true;"
    end

    local Func = string.format([===[
return function(ARENA)
    local min = math.min
    local SIZE = %s
    local PRIMES = { %s };
    local clock = os.clock
    local begin = clock()
    local iter = 0

    while (clock()-begin) <= %s do
        local u = #PRIMES 
        for k = 1, u do
            local v = PRIMES[k]
            local vk = v*2
            for x = (v*v)-vk, SIZE, vk*%s do

                %s

            end
        end
        iter = iter + 1
    end -->loop
    return iter, clock()-begin
end]===],
    tostring(SIZE),
    --1. Primes
    table.concat(PRIMES, ", "),
    --2. Length
    Time or 5,
    --3. Unroll * 2
    Unroll,
    --Unroll*2,
    --4. Inner loop
    Inner
)

    return Func
end

local function CompileAndRun(Time,Unroll, Name, Arena, Size)

    local Func = Compile(Time, Unroll)

    --print(string.format("Generated: Time @ %s, Unroll @ %s", Time, Unroll))
    collectgarbage("collect")
    collectgarbage("stop")

    local Invoke = loadstring(Func, Name)()
    local t1, t2 = Invoke(Arena)

    collectgarbage("restart")
    print(Results(Name, t1, t2, Size))
    return t1,t2

end

if not ARGS[1] then
    print [[
    Mooshua LJ Primes

    Usage:
        luajit prog.lua [mode] [options...]
        Note: Options are specified in order & all must be included.

    Modes:

        o[nce] - Only benchmark once with optimally tuned benchmark parameters

        q[uick] - Quickly benchmark for 5 seconds, and dump output as if testing.

        b[ench] - Automatically run with different options @ 1,000,000 primes.

        e[mit] - Emit lua code for a single benchmark, using arguments for values.
            [time: number] Time to run the benchmark for, in seconds.
            [unroll factor: number] The unroll factor for the code.

        d[ump] - Quickly dump a list of primes to verify correctness (can be checked with test.lua)
    ]]
elseif string.lower(string.sub(ARGS[1],1,1)) == "e" then
    print(Compile(ARGS[2] or 5, ARGS[3] or 1))
elseif string.lower(string.sub(ARGS[1],1,1)) == "o" then

    local ARENA = ffi.new("bool[?]", SIZE + CalcMax(32))
    CompileAndRun(5,32,"mooshua_luajit", ARENA, 8)

elseif string.lower(string.sub(ARGS[1],1,1)) == "q" then
    local ARENA = ffi.new("bool[?]", SIZE)
    local Iters, Time = CompileAndRun(5, 1, "mooshua_lj_quick_donotbench", ARENA, 8)

    local dump = io.open("lj3_primes.txt", "w+")

        if (ARENA[1] == false) then
        dump:write(tostring(1) .. ", ")
    end
    if (ARENA[2] == false) then
        dump:write(tostring(2) .. ", ")
    end
    for i = 3,1000000, 2 do
        if (ARENA[i] == false) then
            dump:write(tostring(i) .. ", ")
        end
    end

    print("Primes dumped to lj3_primes.txt")

elseif string.lower(string.sub(ARGS[1],1,1)) == "b" then
    --  Run multiple benchmarks w/ small unroll counts
    do
        local ARENA = ffi.new("bool[?]", SIZE + CalcMax(32))
        CompileAndRun(5,32,"mooshua_lj_b8_u32", ARENA, 8)
        CompileAndRun(5,24,"mooshua_lj_b8_u24", ARENA, 8)
        CompileAndRun(5,16,"mooshua_lj_b8_u16", ARENA, 8)
        --CompileAndRun(5,4,"mooshua_lj_b8_u4", ARENA, 8)
        CompileAndRun(5,1,"mooshua_lj_b8_u1", ARENA, 8)

    end


    --  With tables
    --  Not a good benchmark, but still an interesting data point.
    do
        local ARENA = {}
        CompileAndRun(5,8,"mooshua_lj_hashtable_8", ARENA, "unknown")
    end

    do
        jit.off()
        local ARENA = {}
        CompileAndRun(5,1,"mooshua_lj_hash_vm", ARENA, "unknown")
    end

elseif string.lower(string.sub(ARGS[1],1,1)) == "d" then
    local ARENA = ffi.new("bool[?]", SIZE)
    local Iters, Time = CompileAndRun(0.0001, 1, "mooshua_lj_dump_donotbench", ARENA, 8)

    local dump = io.open("lj3_primes.txt", "w+")
    if (ARENA[1] == false) then
        dump:write(tostring(1) .. ", ")
    end
    if (ARENA[2] == false) then
        dump:write(tostring(2) .. ", ")
    end
    for i = 3,1000000, 2 do
        if (ARENA[i] == false) then
            dump:write(tostring(i) .. ", ")
        end
    end

    print("Primes dumped to lj3_primes.txt")
end