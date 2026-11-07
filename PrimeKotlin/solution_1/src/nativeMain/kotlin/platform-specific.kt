@file:OptIn(ExperimentalAtomicApi::class)

import kotlinx.coroutines.*
import kotlin.concurrent.atomics.AtomicInt
import kotlin.concurrent.atomics.AtomicReference
import kotlin.concurrent.atomics.ExperimentalAtomicApi
import kotlin.concurrent.atomics.incrementAndFetch
import kotlin.experimental.ExperimentalNativeApi
import kotlin.time.TimeSource

private val timeOrigin = TimeSource.Monotonic.markNow()

actual fun getSystemTimeMillis() = timeOrigin.elapsedNow().inWholeMilliseconds.toDouble()
@OptIn(ExperimentalNativeApi::class)
actual fun defaultCoresCount() = Platform.getAvailableProcessors()
actual fun getArgs() = arrayOf<String>()

actual inline fun <reified T : PrimeSieve> benchmarkPrimeSieve(
    multithreaded: Boolean,
    crossinline newInstance: (Int) -> T
) {
    if (multithreaded) {
        runBlocking(Dispatchers.Default) {
            val iterations = AtomicInt(0)
            val lastIteration = AtomicReference<T?>(null)
            val primeSieveJobs = ArrayList<Job>()

            repeat(CORES) {
                primeSieveJobs += launch {
                    while (isActive) {
                        lastIteration.store(newInstance(PRIME_SIEVE_SIZE))
                        iterations.incrementAndFetch()
                    }
                }
            }

            withContext(Dispatchers.IO) {
                delay(5000)
                primeSieveJobs.forEach { it.cancel() }
                if (!lastIteration.load()!!.isValid) error("Invalid Implementation")
                IterationResult(
                    duration = 5000.0,
                    iterations = iterations.load(),
                    result = lastIteration.load()!!,
                ).print("native", "multi", CORES)
            }
        }
    } else {
        runTest(newInstance).print("native", "single")
    }
}