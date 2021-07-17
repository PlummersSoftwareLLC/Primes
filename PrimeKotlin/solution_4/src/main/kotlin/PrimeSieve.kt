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

class PrimeSieve(private val sieveSize: Int) {
    private val bits = BooleanArray((sieveSize + 1) shr 1) // true = sieved (no prime); false = not sieved (is a prime number)

    fun runSieve() {
        var factor = 3
        val q = sqrt(sieveSize.toDouble()).toInt()
        while (factor < q) {
            var num = factor
            while (num < sieveSize) {
                if (!bits[num shr 1]) {
                    factor = num
                    break
                }
                num += 2
            }

            num = factor * 3
            val incAmount = factor shl 1
            while (num < sieveSize) {
                bits[num shr 1] = true
                num += incAmount
            }

            factor += 2
        }
    }

    fun toString(passes: Int, duration: Double, threads: String) = """
        Passes: $passes, Time: $duration, Avg: ${duration / passes}, Limit: $sieveSize, Count: $primesCount, Valid: $isValid
        
        kotlin_traditional_$threads;$passes;$duration;1;algorithm=base,faithful=yes
    """.trimIndent()

    val primesCount get() = bits.indices.count { !bits[it] }
    val isValid get() = VALIDATION_DATA[sieveSize] == primesCount

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