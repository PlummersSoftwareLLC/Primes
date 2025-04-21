<cfscript>

limit = 1000000;
runTime = 5000;
passes = 0;

// Timed run
start = getTickCount();
sieve = new PrimeSieve(limit);
do {
		sieve.runBitSet();
		passes++;

} while (getTickCount() - start < runTime);

delta = getTickCount() - start;
if (isObject(sieve)) {
	sieve.printResults(delta / 1000, passes);
}

</cfscript>