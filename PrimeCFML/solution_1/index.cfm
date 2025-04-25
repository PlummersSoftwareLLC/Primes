<cfscript>
limit = 1000000;
runTime = 5000;

function benchmark(runFn) {
    var sieve = { instance = "" };
    var passes = 0;
    var start = getTickCount();

    do {
        runFn(sieve);
        passes++;
    } while (getTickCount() - start < runTime);

    // Assume last run result is stored globally in `sieve`
    if (isObject(sieve.instance)) {
        var delta = getTickCount() - start;
        var duration = delta / 1000;

        sieve.instance.printResults(duration, passes);
    }
}

// Run version with Numbers
benchmark(function(sieve) {
    sieve.instance = new PrimeSieveNumbers(limit);
    sieve.instance.run();
});

writeOutput("<br>");

// Run version with Java BitSet
benchmark(function(sieve) {
    sieve.instance = new PrimeSieveBitSet(limit);
    sieve.instance.run();
});


</cfscript>