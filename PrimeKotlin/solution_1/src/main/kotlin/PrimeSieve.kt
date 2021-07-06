import kotlinx.coroutines.*
import java.util.concurrent.atomic.AtomicInteger
import kotlin.math.sqrt

const val n = 1_000_000

fun main() = runBlocking(Dispatchers.Default) {
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

    // SINGLE THREADED

    val startTime = System.currentTimeMillis()

    var iterationsSingleThreaded = 0
    while (System.currentTimeMillis() - startTime < 5000) {
        PrimeSieve(n).runSieve()
        iterationsSingleThreaded++
    }

    val duration = System.currentTimeMillis() - startTime

    println("jakobk_kotlin_singlethreaded;${iterationsSingleThreaded};${duration / 1000.0};1;algorithm=base")
}

class PrimeSieve(private val size: Int) {
    private val bits = BooleanArray((size + 1) / 2) { true }

    fun runSieve() {
        var factor = 3
        while (factor < sqrt(size.toDouble()).toInt()) {
            for (num in factor until size step 2)
                if (bits[num / 2]) {
                    factor = num
                    break
                }

            for (num in factor * 3 until size step factor * 2)
                bits[num / 2] = false

            factor += 2
        }
    }
}
