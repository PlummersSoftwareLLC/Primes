function get(primes, num)
	return bit.band(
		primes[bit.rshift(num, 6)],
		bit.lshift(1, bit.band(bit.rshift(num, 1), 31))
	) == 0
end

local function ruleOut(primes, max)
	local sqrtMax = math.ceil(math.sqrt(max))
	local prime = 3
	while prime <= sqrtMax do
		local step = prime + prime
		for num = prime^2, max, step do
			primes[bit.rshift(num, 6)] = bit.bor(
				primes[bit.rshift(num, 6)],
				bit.band(
					bit.lshift(1, bit.band(bit.rshift(num, 1), 31)),
					-bit.band(num, 1)
				)
			)
		end
		repeat
			prime = prime + 2
		until get(primes, prime)
	end
end

local function getPrimes(max)
	local buf = {max=max}
	for i = 0, max/64 do
		buf[i] = 0
	end

	ruleOut(buf, max)
	return buf
end

local limit, passes = 1000000, 0
local checkNum, realBuf = 78498, {}

-- Lua doesn't have a call to get time more accurate than 1 second, so here I wait until the beginning of a second to give as accurate results as possible.
local startTime, endTime = os.time()+1
while os.time() ~= startTime do end
repeat
	realBuf = getPrimes(limit)
	passes = passes + 1
	endTime = os.time()
until endTime >= startTime+5

local realNum = 0
for i = 1, realBuf.max, 2 do
	if get(realBuf, i) then
		realNum = realNum + 1
	end
end
assert(realNum == checkNum, "realNum: " .. tostring(realNum))
print(string.format("ben1jen_luajit1;%d;%d;1;algorithm=base,faithful=no,bits=1", passes, endTime-startTime))
