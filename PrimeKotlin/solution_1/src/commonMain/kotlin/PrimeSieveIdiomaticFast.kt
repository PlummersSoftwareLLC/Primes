import kotlin.math.sqrt

class PrimeSieveIdiomaticFast(sieveSize: Int) : PrimeSieve(sieveSize) {
    private val dataSet = sieveOfSize(sieveSize)
    private val sqrtOfSieveSize = sqrt(sieveSize.toDouble()).toInt()

    override val foundPrimes get() = dataSet.primes
    override val primesCount get() = dataSet.count

    init {
        runSieve()
    }

    private inline fun fastFor(from: Int, to: Int, step: Int, block: (Int) -> Unit) {
        // Unfortunately the `step` function doesn't get optimized out, so we have to work around this
        var i = from
        while (i <= to) {
            block(i)
            i += step
        }
    }

    private inline fun fastFirst(from: Int, to: Int, step: Int, block: (Int) -> Boolean): Int {
        fastFor(from, to, step) { if (block(it)) return it }
        return -1
    }

    // Nullable numbers are unadvised as they lead to boxing; as a workaround we use -1 as null and create extension
    // functions
    private inline fun <T> Int.alsoIfNotNull(block: (Int) -> T) = also { if (this != -1) block(it) }
    private fun Int.default(default: Int) = if (this == -1) default else this

    private tailrec fun runSieve(factor: Int = 3) {
        if (factor < sqrtOfSieveSize) runSieve(
            (fastFirst(factor, sieveSize, 2) { !dataSet[it] }.alsoIfNotNull { dataSet.clear(it) }.default(factor)) + 2
        )
    }

    private fun SieveMap.clear(factor: Int) =
        fastFor(factor * 3, sieveSize, factor.double) { this[it] = true }

    override val implementationName get() = "idiomatic_fast"
}
