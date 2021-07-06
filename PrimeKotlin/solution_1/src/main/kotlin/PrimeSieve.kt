import kotlinx.coroutines.*
import kotlin.math.sqrt

const val n = 1_000_000

fun main() = runBlocking(Dispatchers.Default) {
    var iterations = 0
    val primeSieveJob = launch {
        while (true) {
            val indices = Array(n) { true } // amount of primes = indices.size - 2
            (2 until sqrt(n.toDouble()).toInt()).map {
                launch { sieveIndex(it, indices) }
            }.joinAll()
            iterations++
        }
    }

    delay(5000)
    primeSieveJob.cancel()
    println("JakobK;$iterations;5;${Runtime.getRuntime().availableProcessors()}")
}

fun sieveIndex(i: Int, indices: Array<Boolean>) {
    if (indices[i]) {
        var j = i * i
        while (j < indices.size) {
            indices[j] = false
            j += i
        }
    }
}
