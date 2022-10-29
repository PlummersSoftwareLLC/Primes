--  This code is a part of Mooshua's LuaJIT contribution to the language drag race.
--  Donated to the public domain (CC0)

local in_dump = {}
local in_truthy = {}

local c_dump = 0
local c_truthy = 0

local c_not_in_dump = 0
local c_not_in_truth = 0

local dump = io.open("lj3_primes.txt", "r"):read("*a")
local truthy = io.open("all_primes", "r"):read("*a")

for num, idx in dump:gmatch("(%d+),") do

    if ( math.random(100) == 1) then
        --print("T:", num, "->", idx)
    end

    in_dump[num] = true
    c_dump = c_dump + 1
end
function Factors(n)
    local f = {}
    for i = 1, n / 2 do
        if n % i == 0 then
            f[#f + 1] = i
        end
    end
    f[#f + 1] = n
    return f
end
for num in truthy:gmatch("(%d+)%s") do
    if not in_dump[num] then
        --print("MISSING FROM DUMP:", num, "(" .. table.concat(Factors(num), ", ") .. ")")
        c_not_in_dump = c_not_in_dump + 1;
    end
    in_truthy[num] = true
    c_truthy = c_truthy + 1
end
for k, v in pairs(in_dump) do
    if not in_truthy[k] then
        c_not_in_truth = c_not_in_truth + 1;
        if (math.random(100) == 1) then
            print("MISSING FROM TRUTH:", k,"(" .. table.concat(Factors(k), ", ") .. ")")
        end
    end
end
print("IN DUMP:", c_dump)
print("IN TRUTH:", c_truthy)
print("*NOT* IN DUMP:", c_not_in_dump)
print("*NOT* IN TRUTH:", c_not_in_truth)
print("DIFF:", c_dump - c_truthy)