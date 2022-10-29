--  LuaJIT Solution 3
--  Made by Mooshua, donated to the public domain (CC0)

local ARGS = {...}
local ffi = require("ffi")
local L1Sieve = require("l1_sieve")

--  =======================
--  Sieve Class
--  Uses pure lua OOP in the form of metamethods
--  =======================

local Sieve = {}

function Sieve.New(o)
    
    local self = o or {}
    self.Unroll = self.Unroll or 1
    self.Name = self.Name or "mooshua_luajit_untitled"
    self.Size = self.Size or 1000000
    self.Time = self.Time or 5
    self.DeoptimizeHashtable = self.DeoptimizeHashtable or false
    self.ExperimentalUnroll = self.ExperimentalUnroll or false

    --  If using FFI variants, protect against memory exceptions by allocating an overrun zone.
    local Overrun = 0
    if (self.DeoptimizeHashtable) then
        Overrun = math.sqrt(self.Size) * self.Unroll
    end

    self.Buffer = self.Buffer or ffi.new("uint8_t[?]", self.Size + Overrun)

    --  apply meta-fun
    return setmetatable(self, {__index = Sieve})
end

function Sieve:Compile()
    
    local Indexer = "rshift( %s , 1)"

    if self.DeoptimizeHashtable then
        Indexer = "%s"
    end

    local Inner = ""
    if (self.ExperimentalUnroll) then
        
        local InnerAppend = ""

        for i = 1, math.floor(self.Unroll/2) do
            --  LuaJIT's CSE is very, very clever. It's somehow figured out that xi and xa both have the same origin (despite being hidden behind a slot load!)
            --  And that the multiplication at the beginning is pointless fodder, so it's optimizing away the dependency-breaker.
            --  The old "black-box" was actually bugged & caused faulty outputs--which was why luajit treated it as a black box in the first place!

            --  I tried floating point numbers. 3.1 + 2 ought to be sufficient, right? WRONG.
            --  The JIT optimized RIGHT over the convert!
            --  7f06ff08  cvttsd2si ebx, [ebx+0x60]
            --  7f06ff0d  add ebp, ebx
            --  7f06ff0f  mov byte [edx+ebp+0x8], 0x1
            --  7f06ff14  add ebp, ebx
            --  7f06ff16  mov byte [edx+ebp+0x8], 0x1
            --  ...

            --  At this point it'd be easier to write a JIT compiler specifically for this loop!
            --  I am unwilling to fight the JIT on this front
            
            Inner = Inner .. "\n\t\tARENA[ ".. string.format("%s", "xi" .. string.rep(" + v", i-1)).." ] = 1"
        
            InnerAppend = InnerAppend .. "\n\t\tARENA[ ".. string.format("%s", "xa" .. string.rep(" + v", i-1)).." ] = 1"
            
        end

        Inner = Inner .. InnerAppend
    else
        for i = 1, self.Unroll do
            --if (i % 2 == 1) then
                Inner = Inner .. "\n\t\tARENA[ ".. string.format("%s", "xi" .. string.rep(" + v", i-1)).." ] = 1;"
            --else
                --alternate: start from half
                --Inner = Inner .. "\n\t\tARENA[ xa" .. string.rep(" + vk", i-1) .." ] = 1;"
            --end
        end
    end

    local Condition = " == 0"

    if (self.Buffer[0] == nil) then
        Condition = " == nil"
    end

    local XA = ""

    if (self.ExperimentalUnroll) then
        --  Luajit is too clever for it's own good.
        --  I need a fast, lightweight "black box" in it's common subexpression elimination
        --  In order to prevent it from optimizing the accesses back into a linear add loop
        XA = "local xa = ".. string.format(Indexer,"x + vp ")
    end

    local Outer = string.format([[
        for k = 3, PRIME_LEN, 2 do
            if ARENA[ %s ] %s then
                --  V:  The add/offset for each non-prime.
                local v = k
                --  VP: The halfway point
                local vp = (k * %s)
                for x = (k*k), SIZE, k*%s do

                    local xi = %s

                    %s

                    %s

                end
            end
        end
    ]], string.format(Indexer, "k"), Condition, self.Unroll, self.Unroll*2, string.format(Indexer, "x", ""), XA, Inner)

    --[[
        local function _get(ARENA,idx)
    local n = bit.rshift(idx,1)
    local shift = bit.band(n, 7)
    local idx = bit.rshift(n, 4)

    return bit.band(bit.rshift(ARENA[idx],shift), 1)
end

local function _set(ARENA, idx, value)
    local n = bit.rshift(idx,1)
    local shift = bit.band(n, 255)
    local negate = bit.bnot(bit.lshift(1, shift))
    local set = bit.lshift( bit.band(value,1), shift)
    local idx = bit.rshift(n, 8)

    ARENA[idx] = bit.bor( bit.band( ARENA[idx], negate ), set )
end
    ]]

    local Func = string.format([===[
local bit = require "bit"
local ffi = require "ffi"
local rshift = bit.rshift
local band = bit.band
local _i32 = ffi.typeof("int32_t")
local _f32 = ffi.typeof("float")

return function(ARENA, TIME)
    local SIZE = %s
    local PRIME_LEN = math.sqrt(SIZE)
    local clock = os.clock
    local begin = clock()
    local iter = 0

    while (clock()-begin) <= TIME do

        %s

        iter = iter + 1
    end -->loop
    return iter, clock()-begin
end]===],
        self.Size,
        Outer
    )

    self.Code = Func

    return self
end

function Sieve:Run()

    self:Compile()
--[[
    local Comp, Err = loadstring(self.Code, self.Name)
    if not Comp then
        print(Err)
    end
    local Func = Comp()
    ]]

    local Func

    if not pcall(function()

        local handle = io.open("compiled/"..self.Name..".lua", "w")

        handle:write(self.Code)

        handle:close()

        Func = require("compiled."..self.Name)

    end) or Func == nil then

        Func = loadstring(self.Code)()

    end

    --  Warm-up the function
    Func(self.Buffer, 0.01)

    --  Collect garbage before running, as not to get mangled by the GC
    --  If we run out of memory, we do it in style!
    collectgarbage("collect")
    collectgarbage("stop")

    local Rounds, Time = Func(self.Buffer, self.Time)

    collectgarbage("restart")
    --  LuaJIT will flush code automatically, but being explicit here
    --  allows us to make sure there is plenty of mcode for new function traces
    jit.flush(Func)

    return Rounds, Time
end

function Sieve:Dump()
    local dump = io.open("lj3_primes.txt", "w+")

    --  Weird condition: Even numbers are not handled by the sieve, but 2 is a prime, so we do this:
    if (self.Buffer[1] == 0) then
        dump:write(tostring(1) .. ", ")
    end
    if (self.Buffer[2] == 0) then
        dump:write(tostring(2) .. ", ")
    end
    for i = 3,1000000, 2 do
        if (self.Buffer[(i-1)/2] == 0) or (self.Buffer[(i-1)/2] == nil) then
            dump:write(tostring(i) .. ", ")
        end
    end

    print("Primes dumped to lj3_primes.txt")
end

function Sieve.Execute(o)

    local s = Sieve.New(o)

    local Rounds, Time = s:Run()
    print(string.format("%s;%s;%s;1;algorithm=base,faithful=no,bits=%s", s.Name, Rounds, Time, o.Bits or ffi.sizeof("bool")*8))

    return s
end


--  =======================
--  Module
--  CLI interface
--  =======================
local Module = {}

--  Emit: Emit lua code for specific bench params
function Module:E()
    print(Sieve.Compile { Time = ARGS[2] or 5, Unroll = ARGS[3] or 1, Size = ARGS[4] or 1000000}.Code)
end

--  Once: Run a simple benchmark
function Module:O()
    Sieve.Execute{ Unroll = 3, Name = "mooshua_luajit"}
end

--  Quickdump: Once + Dump
function Module:Q()
    Sieve.Execute{ Unroll = 6, Name = "mooshua_luajit" }:Dump()
end

function Module:N()
    Sieve.Execute{ Unroll = 4, Name = "mooshua_luajit", Time = 0.1, ExperimentalUnroll = true}:Dump()
end

--  Benchmark: Run multiple benchmarks with different parameters
function Module:B()

--  Removed due to being trampled by the JIT into the standard-style benchmark
--    Sieve.Execute { Unroll = 16, ExperimentalUnroll = true, Name = "mooshua_luajit_d_16"}
--    Sieve.Execute { Unroll = 8, ExperimentalUnroll = true, Name = "mooshua_luajit_d_8"}
--    Sieve.Execute { Unroll = 4, ExperimentalUnroll = true, Name = "mooshua_luajit_d_4"}

    L1Sieve.Execute { Unroll = 16, CacheSize = 8000, Name = "mooshua_luajit_16_c8k"}
    L1Sieve.Execute { Unroll = 16, CacheSize = 16000, Name = "mooshua_luajit_16_c16k"}
    L1Sieve.Execute { Unroll = 16, CacheSize = 24000, Name = "mooshua_luajit_16_c24k"}
    L1Sieve.Execute { Unroll = 16, CacheSize = 32000, Name = "mooshua_luajit_16_c32k"}
    L1Sieve.Execute { Unroll = 16, CacheSize = 48000, Name = "mooshua_luajit_16_c48k"}
    L1Sieve.Execute { Unroll = 16, CacheSize = 64000, Name = "mooshua_luajit_16_c64k"}

    L1Sieve.Execute { Unroll = 8, CacheSize = 24000, Name = "mooshua_luajit_8_c24k"}
    L1Sieve.Execute { Unroll = 8, CacheSize = 32000, Name = "mooshua_luajit_8_c32k"}

    Sieve.Execute { Unroll = 3, Name = "mooshua_luajit"}

    Sieve.Execute { Unroll = 16, Name = "mooshua_luajit_16"}
    Sieve.Execute { Unroll = 8, Name = "mooshua_luajit_8"}
    Sieve.Execute { Unroll = 1, Name = "mooshua_luajit_1"}

    --  Run a hashtable bench
    Sieve.Execute { Name = "mooshua_luajit_hash", Buffer = {} }

    --  Create a benchmark which takes advantage of Lua arrays.
    do
        local buf = {}
        for i = 0, 1000000 / 2 do
            buf[i] = 0
        end
        Sieve.Execute { Name = "mooshua_luajit_array", Buffer = buf }
    end

    --  With optimizations disabled
    jit.opt.start(1)
    Sieve.Execute { Name = "mooshua_luajit_slow_ffi", DeoptimizeHashtable = true }
    Sieve.Execute { Name = "mooshua_luajit_slow_hash", Buffer = {}, Bits=64, DeoptimizeHashtable = true }

    --  Turn the JIT off and repeat
    jit.off()
    Sieve.Execute { Name = "mooshua_luajit_vm_ffi", DeoptimizeHashtable = true }
    --  _hash seems to work better with the optimized lookup :/
    Sieve.Execute { Name = "mooshua_luajit_vm_hash", Buffer = {}, Bits=64, DeoptimizeHashtable = false, }

end

-- Benchmark with different opt settings
function Module:J()
    
    jit.on()

    jit.opt.start(3, "-fold")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_fold"}

    jit.opt.start(3, "-cse")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_cse"}

    jit.opt.start(3, "-dce")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_dce"}


    jit.opt.start(3, "-narrow")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_narrow"}

    jit.opt.start(3, "-fuse")
    Sieve.Execute { Unroll=3, Name = "mooshua_lj_no_fuse" }

    jit.opt.start(3, "-dse")
    Sieve.Execute { Unroll=3, Name = "mooshua_lj_no_store" }

    jit.opt.start(3, "-fwd")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_alias"}

    jit.opt.start(3, "-sink")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_sink"}

    jit.opt.start(3, "-abc")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_array"}

    jit.opt.start(3, "-loop")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_loop"}

    --  Now for L1-optimized

    jit.opt.start(3, "-fold")
    L1Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_fold_c"}

    jit.opt.start(3, "-cse")
    L1Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_cse_c"}

    jit.opt.start(3, "-dce")
    L1Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_dce_c"}


    jit.opt.start(3, "-narrow")
    L1Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_narrow_c"}

    jit.opt.start(3, "-fuse")
    L1Sieve.Execute { Unroll=3, Name = "mooshua_lj_no_fuse_c" }

    jit.opt.start(3, "-dse")
    L1Sieve.Execute { Unroll=3, Name = "mooshua_lj_no_store_c" }

    jit.opt.start(3, "-fwd")
    L1Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_alias_c"}

    jit.opt.start(3, "-sink")
    L1Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_sink_c"}

    jit.opt.start(3, "-abc")
    L1Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_array_c"}

    jit.opt.start(3, "-loop")
    L1Sieve.Execute { Unroll = 1, Name = "mooshua_lj_no_loop_c"}

end

--  Quickly dump a list of primes for testing
function Module:D()
    Sieve.Execute{ Unroll = 16, CacheSize = 64000, Name = "mooshua_lj_donotbench", Time = 0.1, Buffer = {} }:Dump()
end

--  Dump L1 sieve
function Module:L()
    L1Sieve.Execute{ Unroll = 16, CacheSize = 16000, Name = "mooshua_lj_donotbench", Time = 0.1, Buffer = {} }:Dump()
end

--  =======================
--  Main
--  Enter the program & execute CLI based on arguments
--  =======================

if not ARGS[1] then
    print [[
    Mooshua LJ Primes

    Usage:
        luajit prog.lua [mode] [options...]
        Note: Options are specified in order & all must be included.

    Modes:

        *RECOMMENDED*
        b[ench] - Automatically run with different options @ 1,000,000 primes.

        o[nce] - Only benchmark once with optimally tuned benchmark parameters

        q[uick] - Quickly benchmark for 5 seconds, and dump output as if testing.

        j[itanalysis] - Benchmark with same parameters with different JIT opts on/off

        e[mit] - Emit lua code for a single benchmark, using arguments for values.
            [time: number] Time to run the benchmark for, in seconds.
            [unroll factor: number] The unroll factor for the code.
            [size: number] The amount of primes to calculate

        d[ump] - Quickly dump a list of primes to verify correctness (can be checked with test.lua)
    ]]
else

    local mode = string.upper(string.sub(ARGS[1] or " ",1,1))

    if Module[mode] then
        Module[mode](Module)
    else
        print("Error: Unknown option (no args = help message)")
    end
end