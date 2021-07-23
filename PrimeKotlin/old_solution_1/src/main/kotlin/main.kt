import kotlinx.coroutines.*
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import kotlin.system.exitProcess

const val PRIME_SIEVE_SIZE = 1_000_000
const val ITERATION_TIME = 5_000

fun main(args: Array<String>) {
    for (arg in args) {
        when {
            arg.startsWith("-t") -> benchmarkPrimeSieve(arg.endsWith('m')) { PrimeSieveTraditional(it) }
            arg.startsWith("-if") -> benchmarkPrimeSieve(arg.endsWith('m')) { PrimeSieveIdiomaticFast(it) }
            arg.startsWith("-i") -> benchmarkPrimeSieve(arg.endsWith('m')) { PrimeSieveIdiomatic(it) }
            else -> error("Unknown argument $arg")
        }
    }
}

inline fun <reified T : PrimeSieve> benchmarkPrimeSieve(multithreaded: Boolean, crossinline newInstance: (Int) -> T) {
    if (multithreaded) {
        runBlocking(Dispatchers.Default) {
            /// ---------------
            /// MULTI THREADED
            /// ---------------

            val cores = Runtime.getRuntime().availableProcessors()

            val primeSieveJobs = ArrayList<Job>()

            val iterations = AtomicInteger()
            val lastIteration = AtomicReference<T>()

            repeat(cores) {
                primeSieveJobs += launch {
                    while (isActive) {
                        lastIteration.set(newInstance(PRIME_SIEVE_SIZE))
                        iterations.incrementAndGet()
                    }
                }
            }

            withContext(Dispatchers.IO) {
                delay(5000)
                primeSieveJobs.forEach { it.cancel() }
                if (!lastIteration.get().isValid) exitProcess(-1)
                println(lastIteration.get().toString(iterations.get(), ITERATION_TIME / 1000.0, "multi"))
            }

            // cool down
            delay(ITERATION_TIME.toLong())
        }
    } else {
        /// ---------------
        /// SINGLE THREADED
        /// ---------------

        val startTime = System.currentTimeMillis()

        var iterations = 0
        var lastIteration = newInstance(0)
        while (System.currentTimeMillis() - startTime < ITERATION_TIME) {
            lastIteration = newInstance(PRIME_SIEVE_SIZE)
            iterations++
        }

        val duration = System.currentTimeMillis() - startTime

        if (!lastIteration.isValid) exitProcess(-1)
        println(lastIteration.toString(iterations, duration / 1000.0, "single"))
    }
}
