import kotlin.math.sqrt

class PrimeKotlin(private val sieveSize: Int) {
    private val Bits = BooleanArray(sieveSize+1) { true }

    fun runSieve() {
        var factor = 3
        val q = sqrt(sieveSize.toDouble()).toInt()

        while (factor <= q) {
            for (num in factor .. sieveSize step 2) {
                if (Bits[num]) {
                    factor = num
                    break
                }
            }
            var num = factor * factor
            while (num < sieveSize) {
                Bits[num] = false
                num += factor * 2
            }
            factor += 2
        }
    }

    fun printResults(showResults: Boolean, duration: Float, passes: Int) {
        if (showResults) {
            print("2, ")
        }
        var count = if (sieveSize >= 2) 1 else 0
        for (num in 3 .. sieveSize step 2) {
            if (Bits[num]) {
                if (showResults) {
                    print("$num, ")
                }
                count++
            }
        }
        if (showResults) {
            println()
        }
        println("Passes: $passes, Time: $duration, Avg: ${duration / passes}, Limit: ${sieveSize}, Count: $count, Valid: ${validateResults()}")
    }

    private fun validateResults(): Boolean {
        return VALIDATION_DATA[sieveSize] == countPrimes()
    }

    private fun countPrimes(): Int {
        var count = if (sieveSize >= 2) 1 else 0
        var i = 3
        while (i < sieveSize) {
            if (Bits[i]) count++
            i += 2
        }
        return count
    }

    companion object
    {
        val VALIDATION_DATA = mapOf(
            10 to 1,
            100 to 25,
            1_000 to 168,
            10_000 to 1229,
            100_000 to 9592,
            1_000_000 to 78498,
            10_000_000 to 664579,
            100_000_000 to 5761455
        )
    }
}

expect fun currentTimeMillis(): Long

fun doIt(durationMillis: Long) {
    var sieve = PrimeKotlin(3)
    val start = currentTimeMillis()
    var passes = 0
    while (currentTimeMillis() - start < durationMillis) {
        sieve = PrimeKotlin(1_000_000)
        sieve.runSieve()
        passes++
    }
    val delta = currentTimeMillis() - start
    sieve.printResults(false, delta / 1000F, passes)
}

fun main(args: Array<String>) {
    val sec = try {
        args[0].toLong()
    } catch (e: Exception) {
        10_000
    }
    doIt(sec)
}