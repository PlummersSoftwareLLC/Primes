local bit = require "bit"
local ffi = require "ffi"
local rshift = bit.rshift
local band = bit.band
local _i32 = ffi.typeof("int32_t")
local _f32 = ffi.typeof("float")

return function(ARENA, TIME)
    local SIZE = 1000000
    local PRIME_LEN = math.sqrt(SIZE)
    local clock = os.clock
    local begin = clock()
    local iter = 0

    while (clock()-begin) <= TIME do

                for k = 3, PRIME_LEN, 2 do
            if ARENA[ k ]  == nil then
                --  V:  The add/offset for each non-prime.
                local v = k
                --  VP: The halfway point
                local vp = (k * 1)
                for x = (k*k), SIZE, k*2 do

                    local xi = x

                    

                    
		ARENA[ xi ] = 1;

                end
            end
        end
    

        iter = iter + 1
    end -->loop
    return iter, clock()-begin
end