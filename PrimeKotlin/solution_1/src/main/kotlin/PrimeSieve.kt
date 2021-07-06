import kotlinx.coroutines.*
import java.util.concurrent.atomic.AtomicInteger
import kotlin.math.sqrt

const val n = 1_000_000

fun main() {
    runBlocking(Dispatchers.Default) {
        // MULTI THREADED

        val cores = Runtime.getRuntime().availableProcessors()

        val primeSieveJobs = ArrayList<Job>()

        val iterations = AtomicInteger()
        repeat(cores) {
            primeSieveJobs += launch {
                while (isActive) {
                    PrimeSieve(n).runSieve()
                    iterations.incrementAndGet()
                }
            }
        }

        withContext(Dispatchers.IO) {
            delay(5000)
            primeSieveJobs.forEach { it.cancel() }
            println("jakobk_kotlin_coroutines;${iterations.get()};5;${cores};algorithm=base,faithful=yes")
        }

        // cool down
        delay(5000)
    }

    // SINGLE THREADED

    val startTime = System.currentTimeMillis()

    var iterationsSingleThreaded = 0
    while (System.currentTimeMillis() - startTime < 5000) {
        PrimeSieve(n).runSieve()
        iterationsSingleThreaded++
    }

    val duration = System.currentTimeMillis() - startTime

    println("jakobk_kotlin_singlethreaded;${iterationsSingleThreaded};${duration / 1000.0};1;algorithm=base,faithful=yes")
}

class PrimeSieve(private val size: Int) {
    private val bits = BooleanArray((size + 1) shr 1) // true = sieved (no prime); false = not sieved (is a prime number)

    fun runSieve() {
        var factor = 3
        val q = sqrt(size.toDouble()).toInt()
        while (factor < q) {
            var num = factor
            while (num < size) {
                if (!bits[num shr 1]) {
                    factor = num
                    break
                }
                num += 2
            }

            num = factor * 3
            val incAmount = factor shl 1
            while (num < size) {
                bits[num shr 1] = true
                num += incAmount
            }

            factor += 2
        }
    }
}
