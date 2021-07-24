import kotlin.math.sqrt

/**
 * A faithful prime sieve implementation, based off the Java implementation, written in Kotlin.
 *
 * @param sieveSize The amount of primes to calculate for the sieve.
 */
class PrimeSieve(private val sieveSize: Int) {
    private val validationData = mapOf(
        10 to 4,
        100 to 25,
        1000 to 168,
        10000 to 1229,
        100000 to 9592,
        1000000 to 78498,
        10000000 to 664579,
        100000000 to 5761455
    )

    // Java has a BitSet class included but switching to a boolean array of size improves performance by a lot
    // This brings the limitation of the sieve only being able to test numbers up to Integer.MAX_VALUE - 2 (Requested array size exceeds VM limit)
    private var dataSet = BooleanArray(sieveSize + 1 shr 1)

    private fun countPrimes(): Int {
        var count = 0
        for (it in dataSet) if (!it) count++
        return count
    }

    private fun validateResults(): Boolean = if (validationData.containsKey(sieveSize)) {
        validationData[sieveSize] == countPrimes()
    } else false

    // Naming hasn't changed for comparison of the methods but removing the if statement for a branchless comparison
    // and inverting the statement due to not initializing the dataSet to true results in a boost too.
    // Also rather interesting: checking index % 2 != 0 is slower than index % 2 == 1
    private fun getBit(index: Int): Boolean = index and 1 == 1 && !dataSet[index shr 1]

    // Again instead of checking if index is even we just update the array at that index equivalent to that check
    // to boost performance
    private fun clearBit(index: Int) {
        dataSet[index shr 1] = index and 1 == 1
    }

    fun runSieve() {
        var factor = 3
        val q = sqrt(sieveSize.toDouble()).toInt()
        while (factor < q) {
            for (num in factor..sieveSize) {
                if (getBit(num)) {
                    factor = num
                    break
                }
            }
            var num = factor * factor
            while (num <= sieveSize) {
                clearBit(num)
                num += factor * 2
            }
            factor += 2
        }
    }

    fun printResults(showResults: Boolean, duration: Double, passes: Int) {
        if (showResults) print("2, ")
        var count = 1
        for (num in 3..sieveSize) {
            if (getBit(num)) {
                if (showResults) print("$num, ")
                count++
            }
        }
        if (showResults) println()
        println("Passes: $passes, Time: $duration, Avg: ${duration / passes}, Limit: $sieveSize, Count: $count, Valid: ${validateResults()}")
        println()
        println("nhubbard;$passes;$duration;1;algorithm=base,faithful=yes")
    }
}