import kotlin.jvm.JvmInline

fun sieveOfSize(size: Int) = SieveMap(BooleanArray((size + 1).half))

@JvmInline
value class SieveMap(private val sieve: BooleanArray) {
    operator fun get(index: Int) = sieve[index.half]
    operator fun set(index: Int, value: Boolean) {
        sieve[index.half] = value
    }

    val primes get() = sieve.indices.mapNotNull { it.takeIf { sieve[it] } }.toIntArray()
    val count get() = sieve.indices.count { !sieve[it] }
}
