const val PRIME_SIEVE_SIZE = 1_000_000
const val ITERATION_TIME = 5_000

var CORES = defaultCoresCount()

expect fun getSystemTimeMillis(): Double
expect fun defaultCoresCount(): Int
expect fun getArgs(): Array<String>

expect inline fun <reified T : PrimeSieve> benchmarkPrimeSieve(
    multithreaded: Boolean,
    crossinline newInstance: (Int) -> T
)

fun main(args: Array<String>) {
    // Kotlin/JS doesn't translate command line arguments...
    for (arg in if (args.isEmpty()) getArgs() else args) {
        when {
            arg.startsWith("-c") -> { CORES = arg.split('=').last().toInt() }
            arg.startsWith("-t") -> benchmarkPrimeSieve(arg.endsWith('m')) { PrimeSieveTraditional(it) }
            arg.startsWith("-if") -> benchmarkPrimeSieve(arg.endsWith('m')) { PrimeSieveIdiomaticFast(it) }
            arg.startsWith("-i") -> benchmarkPrimeSieve(arg.endsWith('m')) { PrimeSieveIdiomatic(it) }
            else -> error("Unknown argument $arg")
        }
    }
}

data class IterationResult(
    val duration: Double,
    val iterations: Int,
    val result: PrimeSieve,
) {
    fun print(platform: String, threads: String, threadCount: Int = 1) =
        println("${platform}_${result.toString(iterations, duration / 1000.0, threads, threadCount)}")
}

inline fun <reified T : PrimeSieve> runTest(crossinline newInstance: (Int) -> T): IterationResult {
    val startTime = getSystemTimeMillis()

    var iterations = 0
    var lastIteration = newInstance(0)
    while (getSystemTimeMillis() - startTime < ITERATION_TIME) {
        lastIteration = newInstance(PRIME_SIEVE_SIZE)
        iterations++
    }

    if (!lastIteration.isValid) error("Invalid Implementation")
    return IterationResult(
        result = lastIteration,
        duration = getSystemTimeMillis() - startTime,
        iterations = iterations,
    )
}
