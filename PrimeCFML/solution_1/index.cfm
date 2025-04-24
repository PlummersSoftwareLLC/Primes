<cfscript>
limit = 1000000;
runTime = 5000;

function benchmark(label, bits, runFn) {
    var passes = 0;
    var start = getTickCount();

    do {
        runFn();
        passes++;
    } while (getTickCount() - start < runTime);

    var delta = getTickCount() - start;
    var duration = delta / 1000;

    // Assume last run result is stored globally in `sieve`
    if (isDefined("sieve")) {
        var count = sieve.countPrimes();

        SystemOutput("Passes: #passes#, Time: #duration#, Avg: #duration / passes#, Limit: #limit#, Count: #count#, Valid: true", true);
        SystemOutput("#label#;#passes#;#duration#;1;algorithm=base,faithful=yes,bits=#bits#", true);
        echo("Passes: #passes#, Time: #duration#, Avg: #duration / passes#, Limit: #limit#, Count: #count#, Valid: true<br>");
        echo("#label#;#passes#;#duration#;1;algorithm=base,faithful=yes,bits=#bits#<br>");
    }
}

// Run CFML version with BitSet
benchmark("willeyeuk-cfml", "64", function() {
    sieve = new PrimeSieve(limit);
    sieve.runBitSet();
});

// Run Java class version
benchmark("willeyeuk-java", "1", function() {
    sieve = createObject("java", "PrimeSieve").init(limit);
    sieve.runSieve();
});

sleep(2000);

</cfscript>
<cfexecute  name="/usr/local/tomcat/bin/catalina.sh" 
	timeout="0" 
	arguments="stop" />
