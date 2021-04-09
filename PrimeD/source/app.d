import std.datetime;

int[size_t] generateTable()
{
	int[size_t] result;
	result[10] = 4;
	result[100] = 25;
	result[1000] = 168;
	result[10000] = 1229;
	result[100000] = 9592;
	result[1000000] = 78498;
	result[10000000] = 664579;
	result[100000000] = 5761455;
	result[1000000000] = 50847534;
	result[10000000000] = 455052511;
	return result;
}

enum PrimeTable = generateTable();

struct PrimeSieve
{
	size_t sieveSize;
	bool[] bits;
	int[size_t] myDict;

	this(size_t n)
	{
		sieveSize = n;
		import std.range : repeat, take, array;

		bits = true.repeat().take(n).array();
		myDict = PrimeTable;
	}

	int countPrimes()
	{
		int result = 1, i = 3;
		while (i < sieveSize)
		{
			if (bits[i])
				result += 1;
			i += 2;
		}
		return result;
	}

	bool validateResult()
	{
		return (sieveSize in myDict) ? myDict[sieveSize] == countPrimes() : false;
	}

	void runSieve()
	{
		import std.math : sqrt;

		for (size_t factor = 3, q = cast(size_t)(sqrt(cast(float) sieveSize)); factor <= q;
				factor += 2)
		{
			for (auto num = factor; num < sieveSize; num += 2)
			{
				if (bits[num])
				{
					factor = num;
					break;
				}
			}

			for (auto n = factor * factor; n < sieveSize; n += factor * 2)
				bits[n] = false;
		}
	}

	void printResult(bool showResults, long duration, int passes)
	{
		import std.stdio : write, writef, writefln, writeln;

		if (showResults)
			write("2, ");
		int count = 1;
		for (auto num = 3; num <= sieveSize; num += 2)
		{
			if (bits[num])
			{
				count += 1;
				if (showResults)
					writef!"%s, "(num);
			}
		}
		if (showResults)
			writeln();
		writefln!"Passes: %s, Time: %s, Avg: %s, Limit: %s, Count1: %s, Count2: %s, Valid: %s"(passes, duration,
				(float(duration)) / passes, sieveSize, count, countPrimes(), validateResult());
	}
}

void main()
{
	int passes;
	import std.datetime.stopwatch : StopWatch, AutoStart;

	auto sw = StopWatch(AutoStart.yes);
	while (true)
	{
		auto sieve = PrimeSieve(10000000);
		sieve.runSieve();
		++passes;
		const auto elapsed = sw.peek.total!"msecs"();
		if (elapsed >= 5000)
		{
			sieve.printResult(false, elapsed, passes);
			import std.stdio: File, writefln;
			File("../primes.csv", "a").writefln!"D,%s"(passes);
			break;
		}

	}

}
