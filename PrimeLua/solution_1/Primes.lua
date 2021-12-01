-- Based on PrimeCS.cs : Dave's Garage Prime Sieve in C++ (except it was C#)
-- Converted to LUA by SRT
-- OOP all the things so it's faithful by greatwolf

local myDict = 
{   -- rbergen: changed number for 10 from 1 to 4
	k10 = 4 ,        --Historical data for validating our results - the number of primes   
	k100 = 25 ,      --to be found under some limit, such as 168 primes under 1000        
	k1000 = 168 ,
	k10000 = 1229 ,
	k100000 = 9592 ,
	k1000000 = 78498 ,
	k10000000 = 664579 ,
	k100000000 = 5761455 
}

local floor, sqrt = math.floor, math.sqrt

-- roll the metatable up into PrimeSieve to keep things simple
local PrimeSieve = {}
PrimeSieve.__index = PrimeSieve

function PrimeSieve:new(size)
  local bitArray = {}

  for n = 1, floor((size + 1) / 2) do
		bitArray[n] = true
	end

  return setmetatable({sieveSize = size, bitArray = bitArray}, self)
end

function PrimeSieve:countPrimes()
  local count, bitArray = 0, self.bitArray
	for i = 1, #bitArray do
		if bitArray[i] then
			count = count + 1
		end
	end
	return count
end

function PrimeSieve:validateResults()
  local k = 'k' .. self.sieveSize

  return myDict[k] == self:countPrimes()
end

function PrimeSieve:getBit(index)
	if index % 2 == 0 then
		return false
	end
  return self.bitArray[floor(index / 2)]
end

function PrimeSieve:clearBit(index)
	if index % 2 == 0 then
		print("You are setting even bits, which is sub-optimal")
		return
	end
  self.bitArray[floor(index / 2)] = false
end

-- primeSieve
-- 
-- Calculate the primes up to the specified limit

function PrimeSieve:runSieve()
	local factor = 3
  local q = floor(sqrt(self.sieveSize) + 0.5)
  local getBit, clearBit = PrimeSieve.getBit, PrimeSieve.clearBit

	while factor < q do

    for num = factor, self.sieveSize do
      if getBit(self, num) then
        factor = num
        break
      end
    end

		-- If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
		-- We can then step by factor * 2 because every second one is going to be even by definition
		
		-- rbergen: changed start from factor * 3 to factor * factor
    for num = factor * factor, self.sieveSize - 1, factor * 2 do
      clearBit(self, num)
    end

		factor = factor + 2
	end
end

function PrimeSieve:printResults(showResults, duration, passes)
	local s = ""

	if showResults then s = s .. "2, \n" end

	local count = 0
	for _, value in pairs(self.bitArray) do
		if value then count = count + 1 end
	end

	if showResults then s = s .. "\n" end
	s = s .."Passes: " .. passes .. 
		", Time: " .. duration .. 
		", Avg: " .. (duration / passes) .. 
    ", Limit: " .. self.sieveSize .. 
		", Count: " .. count .. 
    ", Valid: " .. tostring(self:validateResults()) 

	-- rbergen: added drag-race output format
  s = s .. "\n\nlua;" .. passes .. ";" .. duration .. ";1;algorithm=base,faithful=yes,bits=64\n"

  print(s)
end

local tStart = os.time()
local passes = 0;

-- rbergen: changed target and duration to drag-race defaults
local target = tonumber(arg[1] or 1000000)
local duration = tonumber(arg[2] or 5)

local sieve
while os.time() - tStart < duration do
    sieve = PrimeSieve:new(target)
    sieve:runSieve()
    passes = passes + 1
end

local tD = os.time() - tStart

sieve:printResults(false, tD, passes)