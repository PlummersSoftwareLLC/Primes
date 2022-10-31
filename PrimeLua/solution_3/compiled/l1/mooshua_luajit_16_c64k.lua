local bit = require "bit"
local ffi = require "ffi"
local rshift = bit.rshift
local band = bit.band
local bor = bit.bor
local _i32_array = ffi.typeof("int32_t[?]")
local _i32 = ffi.typeof("int32_t")
local _f32 = ffi.typeof("float")

return function(ARENA, TIME, CACHE)
    local SIZE = 1000000
    local PRIME_LEN = math.sqrt(SIZE)
    local clock = os.clock
    local begin = clock()
    local iter = 0

    while (clock()-begin) <= TIME do

                local increment_value = ffi.new(_i32_array, PRIME_LEN)
        local last_value = ffi.new(_i32_array, PRIME_LEN)
        local next_free = 0

        --  Determine all primes up to PRIME_LEN 
        for k = 3, PRIME_LEN, 2 do
            if ARENA [ rshift( k , 1) ]  == 0 then
                increment_value[next_free] = k
                last_value[next_free] = (k*k)
                next_free = next_free + 1

                local k2 = k*2
                
                for x = (k*k), PRIME_LEN, k2 do

                    local index = rshift( x , 1)

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
                for x = value, finish, increment*32 do

                    local xi = rshift( x , 1)

                    
		ARENA[ xi ] = 1;
		ARENA[ xi + v ] = 1;
		ARENA[ xi + v + v ] = 1;
		ARENA[ xi + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v + v + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v + v + v + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v + v + v + v + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v + v + v + v + v + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v + v + v + v + v + v + v + v + v + v ] = 1;
		ARENA[ xi + v + v + v + v + v + v + v + v + v + v + v + v + v + v + v ] = 1;

                    last = x

                end
                last_value[factor] = last --math.max(value,last)

            end
        end
        
    

        iter = iter + 1
    end -->loop
    return iter, clock()-begin
end