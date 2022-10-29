local ffi = require("ffi")
local bit = require("bit")

--  ----------------------------
--  L1-optimized sieve extension
--  Made by Mooshua
--  ----------------------------

local Sieve = {}

function Sieve.New(o)
    
    local self = o or {}
    self.Unroll = self.Unroll or 1
    self.Name = self.Name or "mooshua_luajit_untitled"
    self.Size = self.Size or 1000000
    self.Time = self.Time or 5
    --  How many bytes we should use per iteration
    self.CacheSize = self.CacheSize or 24000
    self.DeoptimizeHashtable = self.DeoptimizeHashtable or false
    self.ExperimentalUnroll = self.ExperimentalUnroll or false

    --  If using FFI variants, protect against memory exceptions by allocating an overrun zone.
    local Overrun = 0
    if (self.DeoptimizeHashtable) then
        Overrun = math.sqrt(self.Size) * self.Unroll
    end

    self.Buffer = self.Buffer or ffi.new("uint8_t[?]", self.Size + Overrun)

    for i = 0, self.Size, 1 do
        self.Buffer[i] = 0;
        assert(self.Buffer[i] == 0,i)
    end

    --  apply meta-fun
    return setmetatable(self, {__index = Sieve})
end

function Sieve:Compile()
    
    local Indexer = "rshift( %s , 1)"

    if self.DeoptimizeHashtable then
        Indexer = "%s"
    end

    local Inner = ""
 
    for i = 1, self.Unroll do
        --if (i % 2 == 1) then
            Inner = Inner .. "\n\t\tARENA[ ".. string.format("%s", "xi" .. string.rep(" + v", i-1)).." ] = 1;"
        --else
            --alternate: start from half
            --Inner = Inner .. "\n\t\tARENA[ xa" .. string.rep(" + vk", i-1) .." ] = 1;"
        --end
    end

    local Condition = " == 0"

    if (self.Buffer[0] == nil) then
        Condition = " == nil"
    end



    local Outer = string.format([===[
        local increment_value = ffi.new(_i32_array, PRIME_LEN)
        local last_value = ffi.new(_i32_array, PRIME_LEN)
        local next_free = 0

        --  Determine all primes up to PRIME_LEN 
        for k = 3, PRIME_LEN, 2 do
            if ARENA [ %s ] %s then
                increment_value[next_free] = k
                last_value[next_free] = (k*k)
                next_free = next_free + 1

                local k2 = k*2
                
                for x = (k*k), PRIME_LEN, k2 do

                    local index = %s

                    ARENA[ index ] = 1

                end
            end
        end

        --print("==ALLOCATED")
        --for factor = 0, next_free-1 do
        --    print(increment_value[factor], last_value[factor], factor)
        --end
        --print("==ALLOCATED")

        --  Now, we go over every "segment" of cache
        --  Running the prime sieve along each chunk entirely before starting on the next
        for start = 0, SIZE, CACHE do
            local finish = math.min(start + CACHE, SIZE)
            --print("CACHEBLOCK", start, finish, CACHE, SIZE)

            --  This is our chunk. Iterate over all PRIME_LEN primes.
            for factor = 0, next_free-1 do
                local increment = increment_value[factor]
                local value = last_value[factor]

                if (value > finish) then
                    break
                end

                local v = increment
                local last = 0
                for x = value, finish, increment*%s do

                    local xi = %s

                    %s

                    last = x

                end
                last_value[factor] = last --math.max(value,last)

            end
        end
        
    ]===], string.format(Indexer, "k"), Condition, string.format(Indexer, "x"), self.Unroll*2, string.format(Indexer, "x"), Inner)

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
local bor = bit.bor
local _i32_array = ffi.typeof("int32_t[?]")
local _i32 = ffi.typeof("int32_t")
local _f32 = ffi.typeof("float")

return function(ARENA, TIME, CACHE)
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

        local handle = io.open("compiled/l1/"..self.Name..".lua", "w")

        handle:write(self.Code)

        handle:close()

    end) or Func == nil then

    end

    Func = loadstring(self.Code)()

    --  Warm-up the function
    Func(self.Buffer, 0.01, self.CacheSize)

    --  Collect garbage before running, as not to get mangled by the GC
    --  If we run out of memory, we do it in style!
    collectgarbage("collect")
    collectgarbage("stop")

    local Rounds, Time = Func(self.Buffer, self.Time, self.CacheSize)

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

return Sieve;