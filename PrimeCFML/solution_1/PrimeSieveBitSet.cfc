component {

    // Static validation data
    variables.VALIDATION_DATA = {
        10 = 4,
        100 = 25,
        1000 = 168,
        10000 = 1229,
        100000 = 9592,
        1000000 = 78498,
        10000000 = 664579,
        100000000 = 5761455
    };

    function init(n) {
        this.n = n;
        this.half_n = (n + 1) \ 2;

        this.sieveSet = createObject("java", "java.util.BitSet").init();
        this.sieveSet.set(0, this.half_n + 1); // 0 will cover 2 when counting

        return this;
    }

    function countPrimes() {
        return this.sieveSet.cardinality();
    }

    function validateResults() {
        return structKeyExists(VALIDATION_DATA, this.n) and VALIDATION_DATA[this.n] eq countPrimes();
    }

    function run() {

        var sieve = this.sieveSet;
        var halfLimit = this.half_n;

        for (p = 3; p * p <= this.n; p += 2) {
            var idx = int(p \ 2);
            if (sieve.get(idx)) {
                var start = int((p * p) \ 2);
                for (i = start; i <= halfLimit; i += p) {
                    sieve.clear(i);
                }
            }
        }
    }

    function printResults(duration, passes) {
        var count = this.countPrimes();
        var label = "willeyeuk-bitset";
        var bits =  "1";
        writeOutput("Passes: #passes#, Time: #duration#, Avg: #duration / passes#, Limit: #this.n#, Count: #count#, Valid: #validateResults()#<br>");
        writeOutput("#label#;#passes#;#duration#;1;algorithm=base,faithful=yes,bits=#bits#<br>");
    }
}
