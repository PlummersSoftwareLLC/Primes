--  LuaJIT Solution 3
--  Made by Mooshua, donated to the public domain (CC0)

local ARGS = {...}
local ffi = require("ffi")

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
        for i = 1, math.floor(self.Unroll/2) do
            --if (i ~= math.floor(self.Unroll/2)+1) then
                --prevent overlaps
                Inner = Inner .. "\n\t\tARENA[ ".. string.format(Indexer, "x" .. string.rep(" + vk", i-1)).." ] = 1;"
            --end
            --alternate: start from half
            Inner = Inner .. "\n\t\tARENA[ ".. string.format(Indexer, "xa" .. string.rep(" + vk", i-1)).." ] = 1;"
            
        end
    else
        for i = 1, self.Unroll do
            --if (i % 2 == 1) then
                Inner = Inner .. "\n\t\tARENA[ ".. string.format(Indexer, "x" .. string.rep(" + vk", i-1)).." ] = 1;"
            --else
                --alternate: start from half
                --Inner = Inner .. "\n\t\tARENA[ xa" .. string.rep(" + vk", i-1) .." ] = 1;"
            --end
        end
    end

    local Condition = " == 0"

    if (type(self.Buffer) == "table") then
        Condition = " == null"
    end

    local XA = ""

    if (self.ExperimentalUnroll) then
        XA = "local xa = x + (vk*"..(self.Unroll/2)..")"
    end

    local Outer = string.format([[
        for k = 3, PRIME_LEN, 2 do
            if ARENA[ %s ] %s then
                local v = k--_i32(k)
                local vk = v*2
                for x = (k*k), SIZE, k*%s do

                    %s

                    %s

                end
            end
        end
    ]], string.format(Indexer, "k"), Condition, self.Unroll*2, XA, Inner)

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
local _i32 = ffi.typeof("int32_t")
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

    local handle = io.open("compiled/"..self.Name..".lua", "w")

    handle:write(self.Code)

    handle:close()

    local Func = require("compiled."..self.Name)

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
    Sieve.Execute{ Unroll = 6, Name = "mooshua_luajit"}:Dump()
end

function Module:N()
    Sieve.Execute{ Unroll = 4, Name = "mooshua_luajit", Time = 0.1, ExperimentalUnroll = true}:Dump()
end

--  Benchmark: Run multiple benchmarks with different parameters
function Module:B()

    Sieve.Execute { Unroll = 3, Name = "mooshua_luajit"}


    Sieve.Execute { Unroll = 16, Name = "mooshua_luajit_16"}
    Sieve.Execute { Unroll = 8, Name = "mooshua_luajit_8"}
    Sieve.Execute { Unroll = 1, Name = "mooshua_luajit_1"}

    Sieve.Execute { Unroll = 16, ExperimentalUnroll = true, Name = "mooshua_luajit_d_16"}
    Sieve.Execute { Unroll = 8, ExperimentalUnroll = true, Name = "mooshua_luajit_d_8"}
    Sieve.Execute { Unroll = 4, ExperimentalUnroll = true, Name = "mooshua_luajit_d_4"}

    --  Run a hashtable bench
    Sieve.Execute { Name = "mooshua_luajit_hash", Buffer = {} }

    --  With optimizations disabled
    jit.opt.start(1)
    Sieve.Execute { Name = "mooshua_luajit_slow_ffi", DeoptimizeHashtable = true }
    Sieve.Execute { Name = "mooshua_luajit_slow_hash", Buffer = {}, Bits=64, DeoptimizeHashtable = true }

    --  Turn the JIT off and repeat
    jit.off()
    Sieve.Execute { Name = "mooshua_luajit_vm_ffi", DeoptimizeHashtable = true }
    Sieve.Execute { Name = "mooshua_luajit_vm_hash", Buffer = {}, Bits=64, DeoptimizeHashtable = true, }

    


end

-- Benchmark with different opt settings
function Module:J()
    
    jit.on()

    --[[jit.opt.start(1, "+narrow")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_narrow"}

    jit.opt.start(1, "+fuse")
    Sieve.Execute { Unroll=3, Name = "mooshua_lj_fuse" }

    jit.opt.start(1, "+dse")
    Sieve.Execute { Unroll=3, Name = "mooshua_lj_store" }

    jit.opt.start(1, "+fwd")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_alias"}

    jit.opt.start(1, "+sink")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_sink"}

    jit.opt.start(1, "+abc")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_array"}

    jit.opt.start(1, "+loop")
    Sieve.Execute { Unroll = 1, Name = "mooshua_lj_loop"}]]


    --  Now, inverse

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

end

--  Quickly dump a list of primes for testing
function Module:D()
    Sieve.Execute{ Unroll = 1, Name = "mooshua_lj_donotbench", Time = 0.1, Buffer = {} }:Dump()
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