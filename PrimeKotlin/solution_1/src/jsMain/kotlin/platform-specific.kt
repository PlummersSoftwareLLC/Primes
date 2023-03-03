import kotlinx.atomicfu.atomic
import kotlinx.coroutines.*
import kotlin.js.Date
import kotlin.js.Promise
import node.process.process as nodeProcess

actual fun getSystemTimeMillis() = Date.now()
actual fun defaultCoresCount() = -1

external val process: dynamic


actual fun getArgs() = process.argv.slice(2) as Array<String>

actual inline fun <reified T : PrimeSieve> benchmarkPrimeSieve(
    multithreaded: Boolean,
    crossinline newInstance: (Int) -> T
)  {
    if (multithreaded) {
        error("ERROR: The Kotlin/JS multithreaded implementation is broken, and is disabled to prevent errors.")
        /*
        val primeSieveJobs = ArrayList<Job>()
        println("Created jobs array")
        val iterations = atomic(0)
        println("Created iterations counter")
        val lastIteration = atomic(newInstance(PRIME_SIEVE_SIZE))
        println("Created last iteration atomic reference")

        repeat(CORES) {
            println("Creating job $it")
            primeSieveJobs += CoroutineScope(Dispatchers.Default).launch {
                println("Launched job $it")
                web.timers.setTimeout({
                    println("Finished job $it")
                    cancel()
                }, 5000)
                while (isActive) {
                    lastIteration.value = newInstance(PRIME_SIEVE_SIZE)
                    iterations.incrementAndGet()
                }
            }
        }

        println("Waiting 5s to terminate jobs")
        web.timers.setTimeout({
            println("Terminating jobs")
            primeSieveJobs.forEach { if (!it.isCancelled) it.cancel() }
            println("Confirming validity")
            if (!lastIteration.value.isValid) nodeProcess.exit(-1)

            IterationResult(
                duration = 5000.0,
                iterations = iterations.value,
                result = lastIteration.value
            ).print("js", "multi", CORES)
        }, 5000)
         */
    } else {
        runTest(newInstance).print("js", "single")
    }
}
