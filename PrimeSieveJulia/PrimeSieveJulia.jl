const primeCounts = Dict( 10 => 1,
                          100 => 25,
                          1000 => 168,
                          10000 => 1229,
                          100000 => 9592,
                          1000000 => 78498,
                          10000000 => 664579,
                          100000000 => 5761455 )

struct prime_sieve
	sieveSize::Int
	rawbits::BitArray

	prime_sieve(limit::Int) = new(  limit, trues( floor(Int,(limit+1)/2) )  )
end

function validateResults(sieve::prime_sieve)
	if ( sieve.sieveSize in keys(primeCounts) )
		return primeCounts[sieve.sieveSize] == countPrimes(sieve)
	end

	return false
end

function runSieve!(sieve::prime_sieve)
	factor = 3
	q = sqrt(sieve.sieveSize)

	while factor <= q
		for num in factor:sieve.sieveSize
			if num % 2 == 0 ? false : sieve.rawbits[Int((num+1)/2)]
				factor = num
				break
			end
		end

		for num in factor*3:factor*2:sieve.sieveSize
			sieve.rawbits[Int((num+1)/2)] = false
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
		if sieve.rawbits[Int((num+1)/2)]
			showResults && print("$num" * ", ")

			count += 1
		end
	end

	println(count == countPrimes(sieve))
	println("Passes: $passes, Time: $duration, Avg: $(duration/passes), Limit: $(sieve.sieveSize), Count: $count, Valid: $(validateResults(sieve))")
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
