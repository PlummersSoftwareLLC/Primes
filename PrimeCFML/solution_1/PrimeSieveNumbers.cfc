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

            this.sieveSet = [];
            for (i = 1; i <= this.half_n; i++) {
                this.sieveSet[i] = 1;
            }

        return this;
    }

    function countPrimes() {
        var cnt = 1; // Count 2 as well

        for (i = 1; i <= arrayLen(this.sieveSet); i++) {
            if (this.sieveSet[i]) {
                cnt++;
            }
        }

        return cnt;
    }

    function validateResults() {
        return structKeyExists(VALIDATION_DATA, this.n) and VALIDATION_DATA[this.n] eq countPrimes();
    }

    function run() {

        var sieve = this.sieveSet;
        var halfLimit = this.half_n;

        for (p = 3; p * p <= this.n; p += 2) {
            var idx = int(p \ 2);
            if (sieve[idx]) {
                var start = int((p * p) \ 2);
                for (i = start; i <= halfLimit; i += p) {
                    sieve[i] = 0;
                }
            }
        }
    }

    function printResults(duration, passes) {
        var count = this.countPrimes();
        var label = "willeyeuk-numbers";
        var bits =  "64";
        writeOutput("Passes: #passes#, Time: #duration#, Avg: #duration / passes#, Limit: #this.n#, Count: #count#, Valid: #validateResults()#<br>");
        writeOutput("#label#;#passes#;#duration#;1;algorithm=base,faithful=yes,bits=#bits#<br>");
    }
}
