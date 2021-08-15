const primeCounts = Dict( 10 => 4,
                          100 => 25,
                          1000 => 168,
                          10000 => 1229,
                          100000 => 9592,
                          1000000 => 78498,
                          10000000 => 664579,
                          100000000 => 5761455 )

struct prime_sieve
	sieveSize::Int
	rawbits::BitVector # was BitArray before

	prime_sieve(limit::Int) = new(  limit, trues( div(limit+1,2) )  )
end

function validateResults(sieve::prime_sieve)
	if ( sieve.sieveSize in keys(primeCounts) )
		return primeCounts[sieve.sieveSize] == countPrimes(sieve)
	end

	return false
end

function runSieve!(sieve::prime_sieve)
	factor = 3
	q = trunc(Int, sqrt(sieve.sieveSize))

	while factor <= q
		for num in factor:2:sieve.sieveSize
			if @inbounds sieve.rawbits[div(num+1,2)]
				factor = num
				break
			end
		end

		for idx in div(factor*factor+1,2):div(factor*2+1,2):div(sieve.sieveSize+1,2)
    			@inbounds sieve.rawbits[idx] = false
		end

		factor += 2
	end
	return sieve
end

function countPrimes(sieve::prime_sieve)
	sum(sieve.rawbits)
end

function printResults(sieve::prime_sieve, showResults::Bool, duration::Number, passes::Number)

	showResults && print("2, ")

	count = 1

	for num in 3:2:sieve.sieveSize
		if sieve.rawbits[div(num+1,2)]
			showResults && print("$num" * ", ")

			count += 1
		end
	end

	println(stderr, count == countPrimes(sieve))
	println(stderr, "Passes: $passes, Time: $duration, Avg: $(duration/passes), Limit: $(sieve.sieveSize), Count: $count, Valid: $(validateResults(sieve))")
	println(stderr)
	println("dcbi;$passes;$duration;1;algorithm=base,faithful=yes,bits=1")

end

function rangeTest(factor=3)
	#=
	This function is only valid for odd, positive, integer factors
	=#
	
	sieveSize = 1000000

	# original code used for indexing ########
	numbers = []
	for num in factor*3:factor*2:sieveSize
	append!(numbers, Int((num+1)/2))
	end
	##########################################

	# new code used for indexing #############
	range = div(factor*3+1,2):div(factor*2+1,2):div(sieveSize+1,2)
	rangeNumbers = collect(range)
	##########################################

	return numbers == rangeNumbers, numbers, rangeNumbers
end

function main()
	t0 = time()
	passes = 0

	local sieve

	while (time()-t0) < 5
		sieve = prime_sieve(1000000)
		runSieve!(sieve)
		passes += 1
	end

	tf = time() - t0

	printResults(sieve, false, tf, passes)
end

main()

