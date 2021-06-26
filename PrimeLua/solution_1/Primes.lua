-- Based on PrimeCS.cs : Dave's Garage Prime Sieve in C++ (except it was C#)
-- Converted to LUA by SRT

local sieveSize = 0
local bitArray = {}
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

function setup(size)
	sieveSize = size
	bitArray = {}
	
	for n = 1, math.floor((sieveSize + 1)/2) do
		bitArray[n] = true
	end
end

function countPrimes()
	local count = 0
	for i = 1, #bitArray do
		if bitArray[i] then
			count = count + 1
		end
	end
	return count
end

function validateResults()
	if myDict["k" .. tostring(sieveSize)] then
		return myDict["k" .. tostring(sieveSize)] == countPrimes()
	end
	return nil
end

function getBit(index)
	if index % 2 == 0 then
		return false
	end
	return bitArray[index/2]
end

function clearBit(index)
	if index % 2 == 0 then
		print("You are setting even bits, which is sub-optimal")
		return
	end
	bitArray[math.floor(index / 2)] = false
end

-- primeSieve
-- 
-- Calculate the primes up to the specified limit

function runSieve()
	local factor = 3
	local q = math.floor(math.sqrt(sieveSize)+0.5)
	
	while factor < q do
		
		for num = factor, sieveSize do
			if getBit(num) then
				factor = num
				--continue
			end
		end

		-- If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
		-- We can then step by factor * 2 because every second one is going to be even by definition
		
		-- rbergen: changed start from factor * 3 to factor * factor
		for num = factor * factor, sieveSize-1, factor * 2 do
			clearBit(num)
		end
		
		factor = factor + 2
	end
end

function printResults(showResults, duration, passes)
	local s = ""
	
	if showResults then s = s .. "2, \n" end

	local count = 0
	for _, value in pairs(bitArray) do
		if value then count = count + 1 end
	end
	
	if showResults then s = s .. "\n" end
	s = s .."Passes: " .. passes .. 
		", Time: " .. duration .. 
		", Avg: " .. (duration / passes) .. 
		", Limit: " .. sieveSize .. 
		", Count: " .. count .. 
		", Valid: " .. tostring(validateResults()) 
	
	-- rbergen: added drag-race output format
	s = s .. "\n\nlua;" .. passes .. ";" .. duration .. ";1;algorithm=base,faithful=no,bits=64\n"

	return s
end

local tStart = os.time()
local passes = 0;

-- rbergen: changed target and duration to drag-race defaults
local target = tonumber(arg[1] or 1000000)
local duration = tonumber(arg[2] or 5)

while os.time() - tStart < duration do
    setup(target)
    runSieve()
    passes = passes + 1
end

local tD = os.time() - tStart

print(printResults(false, tD, passes))