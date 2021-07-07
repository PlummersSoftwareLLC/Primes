import kotlin.math.sqrt

expect fun getSystemTimeMillis(): Double
expect val platformName: String

fun main() {
    val sieveSize = 1_000_000
    val runTime = 5_000L

    var passes = 0
    var sieve: PrimeSieve = PrimeSieve(sieveSize).apply { findPrimes() } // warmup init

    val start = getSystemTimeMillis()

    while ((getSystemTimeMillis() - start) < runTime) {
        sieve = PrimeSieve(sieveSize).apply { findPrimes() }
        passes++
    }

    val duration = (getSystemTimeMillis() - start) / 1000.0
    println(
        """Passes: $passes, Time: $duration, Avg: ${
            duration / passes
        }, Limit: $sieveSize, Count: ${sieve.dataSet.primesCount}, Valid: ${sieve.dataSet.isValid}
        
        Kotlin/$platformName Idiomatic;$passes;$duration;1;algorithm=base,faithful=yes
    """.trimIndent()
    )
}

class PrimeSieve(private val sieveSize: Int) {
    val dataSet = SieveMap(sieveSize)
    private val sqrtOfSieveSize = sqrt(sieveSize.toDouble()).toInt()

    tailrec fun findPrimes(factor: Int = 3) {
        if (factor < sqrtOfSieveSize) findPrimes(
            ((factor..sieveSize).firstOrNull { dataSet[it] }?.also { dataSet.clearFactor(it) } ?: factor) + 2
        )
    }

    class SieveMap(private val sieveSize: Int) {
        private val map = BooleanArray((sieveSize + 1) shr 1)
        val primesCount get() = map.indices.count { !map[it] }
        val isValid get() = VALIDATION_DATA[sieveSize] == primesCount

        operator fun get(index: Int) = (index and 1) == 1 && !map[index shr 1]
        fun clearFactor(factor: Int) {
            for (i in (factor * factor)..sieveSize step (factor * 2))
                map[i shr 1] = (i and 1) == 1
        }

        fun toNumbers() = map.indices.mapNotNull { it.takeIf { this[it] } }
        override fun toString() = toNumbers().drop(3).joinToString(prefix = "2, ", separator = ", ")
    }

    companion object {
        val VALIDATION_DATA = mapOf(
            10 to 4,
            100 to 25,
            1_000 to 168,
            10_000 to 1_229,
            100_000 to 9_592,
            1_000_000 to 78_498,
            10_000_000 to 664_579,
            100_000_000 to 5_761_455,
        )
    }
}