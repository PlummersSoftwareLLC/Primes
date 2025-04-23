<cfscript>

limit = 1000000;
runTime = 5000;
passes = 0;

// Timed run
start = getTickCount();

do {
	sieve = new PrimeSieve(limit);
	sieve.runBitSet();
	passes++;

} while (getTickCount() - start < runTime);

delta = getTickCount() - start;
if (isObject(sieve)) {
	sieve.printResults(delta / 1000, passes);
}

   passes = 0; // Instantiate Java class
start = getTickCount();

do {
	    sieve = createObject("java", "PrimeSieve").init(limit);

    sieve.runSieve();
	passes++;

} while (getTickCount() - start < runTime);
delta = getTickCount() - start;
duration = delta / 1000;


		 count = sieve.countPrimes();
		 label = "willeyeuk";
		 bits =  "1";
		SystemOutput("Passes: #passes#, Time: #duration#, Avg: #duration / passes#, Limit: #limit#, Count: #sieve.countPrimes()#, Valid: true", true);
		SystemOutput("#label#;#passes#;#duration#;1;algorithm=base,faithful=yes,bits=#bits#", true);
		echo("Passes: #passes#, Time: #duration#, Avg: #duration / passes#, Limit: #limit#, Count: #sieve.countPrimes()#, Valid: true<br>");
		echo("#label#;#passes#;#duration#;1;algorithm=base,faithful=yes,bits=#bits#<br>");

</cfscript>true