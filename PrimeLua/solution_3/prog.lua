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

    self.Buffer = self.Buffer or ffi.new("bool[?]", self.Size + (math.sqrt(self.Size)*2*self.Unroll))

    --  apply meta-fun
    return setmetatable(self, {__index = Sieve})
end

function Sieve:Compile()
    
    local Inner = ""
    for i = 1, self.Unroll do
        Inner = Inner .. "\n\t\tARENA[ x" .. string.rep(" + vk", i) .." ] = true;"
    end

    local Outer = string.format([[
        for k = 3, PRIME_LEN, 2 do
            if not ARENA[k] then
                local v = k
                local vk = v*2
                for x = (v*v)-vk, SIZE, vk*%s do

                    %s

                end
            end
        end
    ]], self.Unroll, Inner)

    local Func = string.format([===[
local ffi = require "ffi"
return function(ARENA)
    local SIZE = %s
    local PRIME_LEN = math.sqrt(SIZE)
    local clock = os.clock
    local begin = clock()
    local iter = 0

    while (clock()-begin) <= %s do

        %s

        iter = iter + 1
    end -->loop
    return iter, clock()-begin
end]===],
        self.Size,
        self.Time,
        Outer
    )

    self.Code = Func

    return self
end

function Sieve:Run()

    self:Compile()

    local Func = loadstring(self.Code, self.Name)()
    --  Collect garbage before running, as not to get mangled by the GC
    --  If we run out of memory, we do it in style!
    collectgarbage("collect")

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
    if (self.Buffer[1] == false) then
        dump:write(tostring(1) .. ", ")
    end
    if (self.Buffer[2] == false) then
        dump:write(tostring(2) .. ", ")
    end
    for i = 3,1000000, 2 do
        if (self.Buffer[i] == false) then
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
    Sieve.Execute{ Unroll = 16, Name = "mooshua_luajit"}
end

--  Quickdump: Once + Dump
function Module:Q()
    Sieve.Execute{ Unroll = 5, Name = "mooshua_luajit"}:Dump()
end

--  Benchmark: Run multiple benchmarks with different parameters
function Module:B()

    --  Compiler Tuning:
    --  These have been found to slightly increase the performance of the JIT compiler for this workload
    --  (about 5% on a good run)
    --  Your mileage may vary
    if ARGS[2] ~= "notune" then
        jit.opt.start("hotloop=1")
    end
    
    Sieve.Execute { Unroll = 24, Name = "mooshua_luajit_24"}
    Sieve.Execute { Unroll = 16, Name = "mooshua_luajit_16"}
    Sieve.Execute { Unroll = 8, Name = "mooshua_luajit_8"}
    Sieve.Execute { Unroll = 1, Name = "mooshua_luajit_1"}

    --  Run a hashtable bench
    Sieve.Execute { Name = "mooshua_luajit_hash", Buffer = {} }

    --  With optimizations disabled
    jit.opt.start(0)
    Sieve.Execute { Name = "mooshua_luajit_slow_ffi"}
    Sieve.Execute { Name = "mooshua_luajit_slow_hash", Buffer = {}, Bits=64 }

    --  Turn the JIT off and repeat
    jit.off()
    Sieve.Execute { Name = "mooshua_luajit_vm_ffi"}
    Sieve.Execute { Name = "mooshua_luajit_vm_hash", Buffer = {}, Bits=64 }

end

--  Quickly dump a list of primes for testing
function Module:D()
    Sieve.Execute{ Unroll = 1, Name = "mooshua_lj_donotbench", Time = 0 }:Dump()
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
        print("Error: Unknown option")
    end
end