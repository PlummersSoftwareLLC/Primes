import kotlinx.coroutines.*
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import kotlin.system.exitProcess

actual fun getSystemTimeMillis() = System.currentTimeMillis().toDouble()
actual fun defaultCoresCount() = Runtime.getRuntime().availableProcessors()
actual fun getArgs() = arrayOf<String>()

actual inline fun <reified T : PrimeSieve> benchmarkPrimeSieve(
    multithreaded: Boolean,
    crossinline newInstance: (Int) -> T
) {
    if (multithreaded) {
        runBlocking(Dispatchers.Default) {
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

                IterationResult(
                    duration = 5000.0,
                    iterations = iterations.get(),
                    result = lastIteration.get(),
                ).print("native", "multi")
            }
        }

    } else {
        runTest(newInstance).print("native", "single")
    }
}