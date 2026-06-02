component {

    variables.sieveSize = 0;
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

    public PrimeSieve function init( required numeric sieveSize ) {
        variables.sieveSize = arguments.sieveSize;
        variables.bits = arrayNew(1);
        loop from=1 to=variables.sieveSize+1 index="i" {
            arrayAppend( variables.bits, true );
        }
        return this;
    }

    public void function runSieve() {
        var factor = 3;
        var q      = int( sqr( variables.sieveSize ) );

        while ( factor <= q ) {
            var num = factor;
            while ( num <= variables.sieveSize && !variables.bits[ num ] ) {
                num += 2;
            }
            factor = num;

            var i = factor * factor;
            var step = factor * 2;
            while ( i <= variables.sieveSize ) {
                variables.bits[ i ] = false;
                i += step;
            }

            factor += 2;
        }
    }

    public numeric function countPrimes() {
        var count = ( variables.sieveSize >= 2 ) ? 1 : 0;
        var i = 3;
        while ( i <= variables.sieveSize ) {
            if ( variables.bits[ i ] ) count++;
            i += 2;
        }
        return count;
    }

    public boolean function validateResults() {
        var key = javaCast( "string", variables.sieveSize );
        if ( !structKeyExists( variables.primeCounts, key ) ) return false;
        return ( variables.primeCounts[ key ] == countPrimes() );
    }

    public numeric function getSieveSize() {
        return variables.sieveSize;
    }

}
