<cfscript>
    limit      = 1000000;
    runTime    = 5000;
    maxThreads = createObject("java", "java.lang.Runtime").getRuntime().availableProcessors();
    if ( len( trim( server.system.environment.MAX_THREADS ?: "" ) ) && val( server.system.environment.MAX_THREADS ) > 0 ) {
        maxThreads = val( server.system.environment.MAX_THREADS );
    }

    function benchmark(runFn) {
        var sieve  = { instance = "" };
        var passes = 0;
        var start  = getTickCount();
        do {
            runFn(sieve);
            passes++;
        } while (getTickCount() - start < runTime);
        if (isObject(sieve.instance)) {
            var duration = (getTickCount() - start) / 1000;
            sieve.instance.printResults(duration, passes);
        }
    }

    function benchmarkParallel(createFn, threads) {
        var state    = { passes: 0, instance: "" };
        var deadline = getTickCount() + runTime;

        var worker = function(slot) {
            while (true) {

                if (getTickCount() >= deadline) {
                    return;
                }

                var s = createFn();
                s.run();
                state.passes++;
                state.instance = s;
            }
        };

        var slots = [];
        loop from=1 to=threads index="t" {
            arrayAppend(slots, t);
        }

        var start = getTickCount();
        slots.each(closure=worker, parallel=true, maxThreads=threads);
        var duration = (getTickCount() - start) / 1000;

        if (isObject(state.instance)) {
            state.instance.printResults(duration, state.passes, threads);
        }
    }

    benchmark(function(sieve) {
        sieve.instance = new PrimeSieveNumbers(limit);
        sieve.instance.run();
    });
    writeOutput("<br>");

    benchmark(function(sieve) {
        sieve.instance = new PrimeSieveBitSet(limit);
        sieve.instance.run();
    });
    writeOutput("<br>");

    benchmark(function(sieve) {
        sieve.instance = new PrimeSieveBool(limit);
        sieve.instance.run();
    });
    writeOutput("<br>");

    benchmarkParallel(function() {
        return new PrimeSieveNumbers(limit);
    }, maxThreads);
    writeOutput("<br>");

    benchmarkParallel(function() {
        return new PrimeSieveBitSet(limit);
    }, maxThreads);

    writeOutput("<br>");

    benchmarkParallel(function() {
        return new PrimeSieveBool(limit);
    }, maxThreads);
</cfscript>
