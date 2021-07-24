/**
 * The main function to run.
 */
fun main() {
    val start = System.currentTimeMillis()
    var passes = 0
    lateinit var sieve: PrimeSieve
    while ((System.currentTimeMillis() - start) < 5000) {
        sieve = PrimeSieve(1000000)
        sieve.runSieve()
        passes++
    }
    val delta = System.currentTimeMillis() - start
    sieve.printResults(false, delta / 1000.toDouble(), passes)
}