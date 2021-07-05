import kotlin.math.sqrt

fun main() {
    var passes = 0
    var sieve: PrimeSieve = PrimeSieve(1_000_000).apply { findPrimes() } // warmup init

    val start = System.currentTimeMillis()

    while((System.currentTimeMillis() - start) < 10_000L) {
        sieve = PrimeSieve(1_000_000).apply { findPrimes() }
        passes++
    }

    val delta = System.currentTimeMillis() - start
    println(sieve.toString(passes, delta / 1000.0))
}


private class PrimeSieve(private val sieveSize: Int) {
    private val dataSet = SieveMap(sieveSize)
    private val sqrtOfSieveSize = sqrt(sieveSize.toDouble()).toInt()
    private val foundPrimes get() = dataSet::toNumbers

    tailrec fun findPrimes(factor: Int = 3) {
        if (factor < sqrtOfSieveSize) findPrimes(
            ((factor..sieveSize).firstOrNull { dataSet[it] }?.also { dataSet.clear(it) } ?: factor) + 2
        )
    }

    fun toString(passes: Int, duration: Double) = """
        $dataSet
        
        Passes: $passes, Time: $duration, Avg: ${duration / passes}, Limit: $sieveSize, Count: ${dataSet.primesCount}, Valid: ${dataSet.isValid}
        
        wulkanat;$passes;$duration;1;algorithm=base,faithful=yes
    """.trimIndent()


    private class SieveMap(private val sieveSize: Int) {
        private val map = BooleanArray((sieveSize + 1) shr 1)
        val primesCount get() = map.indices.count { !map[it] }
        val isValid get() = VALIDATION_DATA[sieveSize] == primesCount

        operator fun get(index: Int) = (index and 1) == 1 && !map[index shr 1]
        fun clear(factor: Int) {
            for (i in (factor * factor)..sieveSize step (factor * 2))
                map[i shr 1] = (i and 1) == 1
        }

        fun toNumbers() = map.indices.mapNotNull { if (this[it]) it else null }
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