import kotlin.math.sqrt

class PrimeSieveTraditional(sieveSize: Int) : PrimeSieve(sieveSize) {
    private val bits = BooleanArray((sieveSize + 1) shr 1)

    init {
        var factor = 3
        val q = sqrt(sieveSize.toDouble()).toInt()
        while (factor < q) {
            var num = factor
            while (num < sieveSize) {
                if (!bits[num shr 1]) {
                    factor = num
                    break
                }
                num += 2
            }

            num = factor * 3
            val incAmount = factor shl 1
            while (num < sieveSize) {
                bits[num shr 1] = true
                num += incAmount
            }

            factor += 2
        }
    }

    override val primesCount get() = bits.indices.count { !bits[it] }
    override val foundPrimes get() = bits.indices.mapNotNull { it.takeIf { bits[it] } }.toIntArray()

    override val implementationName get() = "traditional"
}
