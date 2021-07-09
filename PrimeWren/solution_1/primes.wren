var DICT = {
    10: 4,
    100: 25,
    1000: 168,
    10000: 1229,
    100000: 9592,
    1000000: 78498,
    10000000: 664579,
    100000000: 5761455,
    1000000000: 50847534,
    10000000000: 455052511,
}

class PrimeSieve {
    construct new(sieveSize) {
        _sieveSize = sieveSize
        _bits = List.filled(sieveSize, false)
    }

    runSieve() {
        var factor = 3
        var q = _sieveSize.sqrt

        while(factor <= q) {
            var num = factor
            while(num < _sieveSize) {
                if(!_bits[num]) {
                    factor = num
                    break
                }

                num = num + 2
            }

            var num2 = factor * factor
            while(num2 < _sieveSize) {
                _bits[num2] = true
                num2 = num2 + factor * 2
            }

            factor = factor + 2
        }
    }

    printResults(showResults, duration, passes) {
        if(showResults) System.write("2, ")

        var count = 1
        var num = 3
        while(num < _sieveSize) {
            if(!_bits[num]) {
                if(showResults) System.write("%(num), ")
                count = count + 1
            }

            num = num + 2
        }

        if(showResults) System.print("")

        var avg = duration / passes
        var countPrimes = this.countPrimes()
        var valid = this.validateResults()

        // System.print("Passes: %(passes), Time: %(duration), Avg: %(avg), Limit: %(_sieveSize), Count1: %(countPrimes), Count2: %(count), Valid: %(valid)")
        System.print("marghidanu;%(passes);%(duration);1;algorithm=base,faithful=yes")
    }

    countPrimes() {
        var count = 1

        var num = 3
        while(num < _sieveSize) {
            if (!_bits[num]) count = count + 1
            num = num + 2
        }

        return count
    }

    validateResults() {
        return DICT[_sieveSize] == this.countPrimes()
    }
}

var passes = 0
var startTime = System.clock

while(true) {
    var sieve = PrimeSieve.new(1000000)
    sieve.runSieve()

    passes = passes + 1
    var duration = System.clock - startTime
    if (duration >= 5) {
        sieve.printResults(false, duration, passes)
        break
    }
}