import kotlinx.coroutines.*
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import kotlin.math.sqrt

const val PRIME_SIEVE_SIZE = 1_000_000
const val ITERATION_TIME = 5_000
const val SKIP_MULTI_THREAD = false

fun main() {
    if (!SKIP_MULTI_THREAD) {
        /// ---------------
        /// MULTI THREADED
        /// ---------------

        runBlocking(Dispatchers.Default) {
            // MULTI THREADED

            val cores = Runtime.getRuntime().availableProcessors()

            val primeSieveJobs = ArrayList<Job>()

            val iterations = AtomicInteger()
            val lastIteration = AtomicReference<PrimeSieve>()

            repeat(cores) {
                primeSieveJobs += launch {
                    while (isActive) {
                        lastIteration.set(PrimeSieve(PRIME_SIEVE_SIZE).apply { runSieve() })
                        iterations.incrementAndGet()
                    }
                }
            }

            withContext(Dispatchers.IO) {
                delay(5000)
                primeSieveJobs.forEach { it.cancel() }
                println(lastIteration.get().toString(iterations.get(), ITERATION_TIME / 1000.0, "multithreaded"))
            }

            // cool down
            delay(ITERATION_TIME.toLong())
        }
    }

    /// ---------------
    /// SINGLE THREADED
    /// ---------------

    val startTime = System.currentTimeMillis()

    var iterations = 0
    var lastIteration = PrimeSieve(0)
    while (System.currentTimeMillis() - startTime < ITERATION_TIME) {
        lastIteration = PrimeSieve(PRIME_SIEVE_SIZE).apply { runSieve() }
        iterations++
    }

    val duration = System.currentTimeMillis() - startTime

    println(lastIteration.toString(iterations, duration / 1000.0, "singlethreaded"))
}

inline val Int.double get() = this shl 1
inline val Int.half get() = this shr 1

private class PrimeSieve(private val sieveSize: Int) {
    private val dataSet = SieveMap(sieveSize)
    private val sqrtOfSieveSize = sqrt(sieveSize.toDouble()).toInt()
    private val foundPrimes get() = dataSet.toNumbers()

    tailrec fun runSieve(factor: Int = 3) {
        if (factor < sqrtOfSieveSize) runSieve(
            ((factor..sieveSize step 2).firstOrNull { !dataSet[it] }?.also { dataSet.clear(it) } ?: factor) + 2
        )
    }

    fun toString(passes: Int, duration: Double, threads: String) = """
        Passes: $passes, Time: $duration, Avg: ${duration / passes}, Limit: $sieveSize, Count: ${dataSet.primesCount}, Valid: ${dataSet.isValid}
        
        kotlin_idiomatic_$threads;$passes;$duration;1;algorithm=base,faithful=yes
    """.trimIndent()


    private class SieveMap(private val sieveSize: Int) {
        private val map = BooleanArray((sieveSize + 1).half)
        val primesCount get() = map.indices.count { !map[it] }
        val isValid get() = VALIDATION_DATA[sieveSize] == primesCount

        operator fun get(index: Int) = map[index.half]
        operator fun set(index: Int, value: Boolean) { map[index.half] = value }
        fun clear(factor: Int) {
            for (i in factor * 3..sieveSize step factor.double) this[i] = true
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