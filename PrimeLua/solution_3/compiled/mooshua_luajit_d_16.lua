local bit = require "bit"
local ffi = require "ffi"
local rshift = bit.rshift
local _i32 = ffi.typeof("int32_t")
return function(ARENA, TIME)
    local SIZE = 1000000
    local PRIME_LEN = math.sqrt(SIZE)
    local clock = os.clock
    local begin = clock()
    local iter = 0

    while (clock()-begin) <= TIME do

                for k = 3, PRIME_LEN, 2 do
            if ARENA[ rshift( k , 1) ]  == 0 then
                local v = k--_i32(k)
                local vk = v*2
                for x = (k*k), SIZE, k*32 do

                    local xa = x + (vk*8)

                    
		ARENA[ rshift( x , 1) ] = 1;
		ARENA[ rshift( xa , 1) ] = 1;
		ARENA[ rshift( x + vk , 1) ] = 1;
		ARENA[ rshift( xa + vk , 1) ] = 1;
		ARENA[ rshift( x + vk + vk , 1) ] = 1;
		ARENA[ rshift( xa + vk + vk , 1) ] = 1;
		ARENA[ rshift( x + vk + vk + vk , 1) ] = 1;
		ARENA[ rshift( xa + vk + vk + vk , 1) ] = 1;
		ARENA[ rshift( x + vk + vk + vk + vk , 1) ] = 1;
		ARENA[ rshift( xa + vk + vk + vk + vk , 1) ] = 1;
		ARENA[ rshift( x + vk + vk + vk + vk + vk , 1) ] = 1;
		ARENA[ rshift( xa + vk + vk + vk + vk + vk , 1) ] = 1;
		ARENA[ rshift( x + vk + vk + vk + vk + vk + vk , 1) ] = 1;
		ARENA[ rshift( xa + vk + vk + vk + vk + vk + vk , 1) ] = 1;
		ARENA[ rshift( x + vk + vk + vk + vk + vk + vk + vk , 1) ] = 1;
		ARENA[ rshift( xa + vk + vk + vk + vk + vk + vk + vk , 1) ] = 1;

                end
            end
        end
    

        iter = iter + 1
    end -->loop
    return iter, clock()-begin
end