import kotlin.math.sqrt

class PrimeSieveIdiomatic(sieveSize: Int) : PrimeSieve(sieveSize) {
    private val dataSet = sieveOfSize(sieveSize)
    private val sqrtOfSieveSize = sqrt(sieveSize.toDouble()).toInt()

    override val foundPrimes get() = dataSet.primes
    override val primesCount get() = dataSet.count

    init {
        runSieve()
    }

    private tailrec fun runSieve(factor: Int = 3) {
        if (factor < sqrtOfSieveSize) runSieve(
            ((factor..sieveSize step 2).firstOrNull { !dataSet[it] }?.also { dataSet.clear(it) } ?: factor) + 2
        )
    }

    private fun SieveMap.clear(factor: Int) {
        for (i in factor * 3..sieveSize step factor.double) this[i] = true
    }

    override val implementationName get() = "idiomatic"
}