local bit = require "bit"
local ffi = require "ffi"
local _i32 = ffi.typeof("int32_t")
return function(ARENA)
    local SIZE = 1000000
    local PRIME_LEN = math.sqrt(SIZE)
    local clock = os.clock
    local begin = clock()
    local iter = 0

    while (clock()-begin) <= 5 do

                for k = 3, PRIME_LEN, 2 do
            if ARENA[bit.rshift(k,1)]  == 0 then
                local v = k--_i32(k)
                local vk = v*2
                for x = (k*k), SIZE, k*6 do

                    

                    
		ARENA[ bit.rshift(x,1) ] = 1;
		ARENA[ bit.rshift(x + vk,1) ] = 1;
		ARENA[ bit.rshift(x + vk + vk,1) ] = 1;

                end
            end
        end
    

        iter = iter + 1
    end -->loop
    return iter, clock()-begin
end