inline val Int.double get() = this shl 1
inline val Int.half get() = this shr 1

abstract class PrimeSieve(val sieveSize: Int) {
    abstract val primesCount: Int
    abstract val foundPrimes: IntArray
    abstract val implementationName: String

    fun toString(passes: Int, duration: Double, threads: String, threadCount: Int) =
        "kotlin_${implementationName}_$threads;$passes;$duration;$threadCount;algorithm=base,faithful=yes"

    val isValid get() = VALIDATION_DATA[sieveSize] == primesCount

    companion object {
        val VALIDATION_DATA = mapOf(
            10 to 4,
            100 to 25,
            1_000 to 168,
            10_000 to 1_229,
            100_000 to 9_592,
            1_000_000 to 78_498,
            10_000_000 to 664_579,
            100_000_000 to 5_761_455,
        )
    }
}