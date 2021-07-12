
local limit, passes = 1000000, 0
local checkNum, buf = 78498

local AND, OR, rshift, lshift = bit.band, bit.bor, bit.rshift, bit.lshift
local ceil, sqrt, floor = math.ceil, math.sqrt, math.floor
local ffi = require "ffi"
local C = ffi.C

-- Timer:
local getTime
if jit.os == "Windows" then
	ffi.cdef[[
	bool QueryPerformanceCounter(int64_t *lpPerformanceCount);
	bool QueryPerformanceFrequency(int64_t *lpFrequency);
	]]

	local frequency = ffi.new("int64_t[1]")
	C.QueryPerformanceFrequency(frequency)
	local now = ffi.new("int64_t[1]")

	function getTime()
		C.QueryPerformanceCounter(now)
		return tonumber(now[0]) / tonumber(frequency[0])
	end
else	-- Linux
	ffi.cdef [[
	typedef struct timespec {
		long	tv_sec;
		long	tv_nsec;
	} nanotime;
	int clock_gettime(int clk_id, struct timespec *tp);
	]]

	local pnano = ffi.new("nanotime[1]")

	function getTime()
		C.clock_gettime(1, pnano)
		return tonumber(pnano[0].tv_sec) + (tonumber(pnano[0].tv_nsec) / 1e+9)
	end
end

local Prime = {}
Prime.__index = Prime

function Prime.new()
	return setmetatable({arr=ffi.new("int[?]", limit/64)}, Prime)
end

function Prime:get(num)
	return AND(
		self.arr[rshift(num, 6)],
		lshift(1, AND(rshift(num, 1), 31))
	) == 0
end

function Prime:getPrimes(max)
	local sqrtMax = ceil(sqrt(max))
	local prime = 3
	while prime <= sqrtMax do
		local step = prime + prime
		for num = prime^2, max, step do
			self.arr[rshift(num, 6)] = OR(
				self.arr[rshift(num, 6)],
				AND(
					lshift(1, AND(rshift(num, 1), 31)),
					-AND(num, 1)
				)
			)
		end
		repeat
			prime = prime + 2
		until self:get(prime)
	end
	return primes
end

local startTime = getTime()
local endTime = startTime + 5

repeat
	buf = Prime.new()
	buf:getPrimes(limit)
	passes = passes + 1
until getTime() > endTime

local realNum = 0
for i = 1, limit, 2 do
	if buf:get(i) then
		realNum = realNum + 1
	end
end
assert(realNum == checkNum, "realNum: " .. tostring(realNum))
print(string.format("mipett_luajit;%d;%d;1;algorithm=base,faithful=no,bits=1", passes, tonumber(endTime-startTime)))
