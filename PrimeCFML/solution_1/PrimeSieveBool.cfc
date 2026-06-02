component {

    variables.sieveSize = 0;
    variables.halfSize  = 0;
    variables.bits      = [];

    variables.primeCounts = {
        "10"        : 4,
        "100"       : 25,
        "1000"      : 168,
        "10000"     : 1229,
        "100000"    : 9592,
        "1000000"   : 78498,
        "10000000"  : 664579,
        "100000000" : 5761455
    };

    public PrimeSieveBool function init( required numeric sieveSize ) {
        variables.sieveSize = arguments.sieveSize;
        variables.halfSize  = int( (arguments.sieveSize + 1) / 2 );
        variables.bits      = arrayNew(1);
        loop from=1 to=variables.halfSize index="i" {
            arrayAppend( variables.bits, true );
        }
        return this;
    }

    public void function run() {
        var bits      = variables.bits;
        var halfSize  = variables.halfSize;
        var sieveSize = variables.sieveSize;
        var q         = int( sqr( sieveSize ) );
        var factor    = 3;

        while ( factor <= q ) {

            var idx = factor \ 2;
            while ( idx <= halfSize && !bits[ idx ] ) {
                idx++;
                factor += 2;
            }

            var i    = int( (factor * factor) / 2 );
            var step = factor;
            while ( i <= halfSize ) {
                bits[ i ] = false;
                i += step;
            }

            factor += 2;
        }
    }

    public numeric function countPrimes() {
        var bits     = variables.bits;
        var halfSize = variables.halfSize;
        var count    = ( variables.sieveSize >= 2 ) ? 1 : 0;
        var i        = 1;
        while ( i <= halfSize ) {
            if ( bits[ i ] ) count++;
            i++;
        }
        return count;
    }

    public boolean function validateResults() {
        var key = javaCast( "string", variables.sieveSize );
        if ( !structKeyExists( variables.primeCounts, key ) ) return false;
        return ( variables.primeCounts[ key ] == countPrimes() );
    }

    public function printResults(duration, passes, threads=1) {
        var count = this.countPrimes();
        var label = (threads > 1) ? "willeyeuk-booleans-parallel" : "willeyeuk-booleans";
        var bits  = "64";
        writeOutput("Passes: #passes#, Time: #duration#, Avg: #duration / passes#, Limit: #variables.sieveSize#, Count: #count#, Valid: #validateResults()#<br>");
        writeOutput("#label#;#passes#;#duration#;#threads#;algorithm=base,faithful=yes,bits=#bits#<br>");
    }

}
