import kotlinx.coroutines.*
import kotlin.system.getTimeMillis

actual fun getSystemTimeMillis() = getTimeMillis().toDouble()
actual fun defaultCoresCount() = 6
actual fun getArgs() = arrayOf<String>()

actual inline fun <reified T : PrimeSieve> benchmarkPrimeSieve(
    multithreaded: Boolean,
    crossinline newInstance: (Int) -> T
) {
    runBlocking {
        if (multithreaded) {
            error("The Kotlin/Native Multithreading implementation is incomplete.")
            /*val iterations = AtomicInt(0)
            val lastIteration = AtomicReference(newInstance(PRIME_SIEVE_SIZE))
            val primeSieveJobs = ArrayList<Job>()

            repeat(CORES) {
                primeSieveJobs += launch {
                    while (isActive) {
                        /*lastIteration.value = */newInstance(PRIME_SIEVE_SIZE)
                        iterations.increment()
                    }
                }
            }

            delay(5000)
            primeSieveJobs.forEach { it.cancel() }

            if (!lastIteration.value.isValid) error("Invalid Implementation")
            IterationResult(
                duration = 5000.0,
                iterations = iterations.value,
                result = lastIteration.value,
            ).print("native", "multi")*/
        } else {
            runTest(newInstance).print("native", "single")
        }
    }
}