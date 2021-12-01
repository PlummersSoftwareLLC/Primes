local ffi = require "ffi"
local lshift, rshift, band, bor = bit.lshift, bit.rshift, bit.band, bit.bor
local ceil, sqrt = math.ceil, math.sqrt
local time = os.time

local function get(primes, num)
	return band(
		primes[rshift(num, 6)],
		lshift(1, band(rshift(num, 1), 31))
	) == 0
end

local function ruleOut(primes, max)
	local sqrtMax = ceil(sqrt(max))
	local prime = 3
	while prime <= sqrtMax do
		local step = prime + prime
		for num = prime^2, max, step do
			primes[rshift(num, 6)] = bor(
				primes[rshift(num, 6)],
				band(
					lshift(1, band(rshift(num, 1), 31)),
					-band(num, 1)
				)
			)
		end
		repeat
			prime = prime + 2
		until get(primes, prime)
	end
end

local function getPrimes(max)
	local buf = ffi.new("int[?]", max/64)
	ruleOut(buf, max)
	return buf
end

local limit, passes = 1000000, 0
local checkNum, realBuf = 78498

-- Lua doesn't have a call to get time more accurate than 1 second, so here I wait until the beginning of a second to give as accurate results as possible.
local startTime, endTime = time()+1
while time() ~= startTime do end
repeat
	realBuf = getPrimes(limit)
	passes = passes + 1
	endTime = time()
until endTime >= startTime+5

local realNum = 0
for i = 1, limit, 2 do
	if get(realBuf, i) then
		realNum = realNum + 1
	end
end
assert(realNum == checkNum, "realNum: " .. tostring(realNum))
print(string.format("ben1jen_luajit1;%d;%d;1;algorithm=base,faithful=no,bits=1", passes, endTime-startTime))
