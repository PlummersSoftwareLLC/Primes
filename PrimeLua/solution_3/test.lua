--  This code is a part of Mooshua's LuaJIT contribution to the language drag race.
--  Donated to the public domain (CC0)

local in_dump = {}
local in_truthy = {}

local c_dump = 0
local c_truthy = 0

local dump = io.open("lj3_primes.txt", "r"):read("*a")
local truthy = io.open("all_primes", "r"):read("*a")

for num in dump:gmatch("(%d+),") do
    in_dump[num] = true
    c_dump = c_dump + 1
end

for num in truthy:gmatch("(%d+)%s") do
    if not in_dump[num] then
        print("MISSING FROM DUMP:", num)
    end
    in_truthy[num] = true
    c_truthy = c_truthy + 1
end

for k, v in pairs(in_dump) do
    if not in_truthy[k] then
        print("MISSING FROM TRUTH:", k)
    end
end

print("IN DUMP:", c_dump)
print("IN TRUTH:", c_truthy)

print("DIFF:", c_dump - c_truthy)