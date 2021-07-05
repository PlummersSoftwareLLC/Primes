import kotlin.math.sqrt

fun main() {
    var passes = 0
    var sieve = PrimeSieveFast(1_000_000) // warmup init

    val start = System.currentTimeMillis()

    while ((System.currentTimeMillis() - start) < 10_000L) {
        sieve = PrimeSieveFast(1_000_000)
        passes++
    }

    val delta = System.currentTimeMillis() - start
    println(sieve.toString(passes, delta / 1000.0))
}

private class PrimeSieveFast(private val sieveSize: Int) {
    private val sqrtOfSieveSize = sqrt(sieveSize.toDouble()).toInt()
    private val map = BooleanArray((sieveSize + 1) shr 1)
    private val foundPrimes get() = map.indices.mapNotNull { if (this[it]) it else null }

    val primesCount get() = map.indices.count { !map[it] }
    val isValid get() = VALIDATION_DATA[sieveSize] == primesCount

    init {
        var factor = 3

        while (factor < sqrtOfSieveSize) {
            factor = ((factor..sieveSize).firstOrNull { this[it] }?.also { clear(it) } ?: factor) + 2
        }
    }

    operator fun get(index: Int) = (index and 1) == 1 && !map[index shr 1]

    fun clear(factor: Int) {
        val step = factor * 2
        var i = factor * factor
        while (i <= sieveSize) {
            map[i shr 1] = (i and 1) == 1
            i += step
        }
    }

    fun toString(passes: Int, duration: Double) = """
        ${foundPrimes.drop(3).joinToString(prefix = "2, ", separator = ", ")}
        
        Passes: $passes, Time: $duration, Avg: ${duration / passes}, Limit: $sieveSize, Count: $primesCount, Valid: $isValid
        
        wulkanat;$passes;$duration;1;algorithm=base,faithful=yes
    """.trimIndent()

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
