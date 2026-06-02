<cfscript>

    sieveLimit = 1000000;
    runSeconds = 5;

    maxThreads = 16;
    if ( len( trim( url.maxThreads ?: "" ) ) ) {
        maxThreads = val( url.maxThreads );
    } else if ( len( trim( server.system.environment.MAX_THREADS ?: "" ) ) ) {
        maxThreads = val( server.system.environment.MAX_THREADS );
    }
    maxThreads = max( 1, maxThreads );

    state = { passes: 0, lastSieve: "" };

    deadline = getTickCount() + ( runSeconds * 1000 );

    workerClosure = function( slot ) {
        while ( true ) {

            if ( getTickCount() >= deadline ) {
                return;
            }

            var sieve = new PrimeSieve( sieveLimit );
            sieve.runSieve();
            state.passes++;
            state.lastSieve = sieve;

        }
    };


    slots = [];
    loop from=1 to=maxThreads index="t" {
        arrayAppend( slots, t );
    }

    tStart = getTickCount();

    slots.each(
        closure    = workerClosure,
        parallel   = true,
        maxThreads = maxThreads
    );

    duration = ( getTickCount() - tStart ) / 1000;

    passes = state.passes;
    sieve  = state.lastSieve;
    valid  = ( isObject( sieve ) && sieve.validateResults() );
    count  = isObject( sieve ) ? sieve.countPrimes() : 0;

    writeOutput(
        "Passes: #passes#, " &
        "Time: #numberFormat( duration, '0.000000' )#, " &
        "Avg: #numberFormat( duration / max( passes, 1 ), '0.000000' )#, " &
        "Limit: #sieveLimit#, " &
        "Count: #count#, " &
        "Valid: #valid#"
    );
    writeOutput( chr(10) & chr(10) );

    writeOutput(
        "willeyeuk-threaded;" &
        "#passes#;" &
        "#numberFormat( duration, '0.000000' )#;" &
        "#maxThreads#;" &
        "algorithm=base,faithful=yes,bits=1"
    );
    writeOutput( chr(10) );
</cfscript>
