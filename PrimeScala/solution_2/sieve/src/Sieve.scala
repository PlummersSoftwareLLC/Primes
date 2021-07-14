import Math.sqrt
import scala.annotation.tailrec

final class Sieve(val size: Int) {
  // Allocate one extra element for sentinel value
  private[this] val bits: Array[Boolean] =
    new Array[Boolean]((size + 1) / 2 + 1)
  bits(bits.size - 1) = true

  private[this] def markBit(index: Int): Unit = bits(index >> 1) = true

  private[this] def notPrime(k: Int): Boolean = bits(k >> 1)

  @tailrec
  private[this] def nextPrime(i: Int): Int = {
    if (notPrime(i)) nextPrime(i + 2) else i
  }

  @tailrec
  private[this] def markMultiples(i: Int, increment: Int): Unit = {
    if (i < size) {
      markBit(i)
      markMultiples(i + increment, increment)
    } else ()
  }

  def run(): Unit = {
    var factor = 3
    val upperBound = (sqrt(size) + 1).toInt

    while (factor < upperBound) {
      markMultiples(factor * factor, factor * 2)
      factor = nextPrime(factor + 2)
    }
  }

  def primeCount: Int = bits.take(size / 2 + 1).count(_ == false)

  def isPrime(k: Int): Boolean = k % 2 == 1 && !notPrime(k)

  def getPrimes = Seq(2) ++ (for (i <- 3 until size if isPrime(i)) yield i)

}

object Sieve {
  val sieveSize = 1000000
  val runTimeMs: Long = 5000

  def main(args: Array[String]): Unit = {
    val t0 = System.currentTimeMillis
    var passes = 0

    while (System.currentTimeMillis() - t0 < runTimeMs) {
      val sieve = new Sieve(sieveSize)
      sieve.run()
      passes += 1
    }

    val dt = System.currentTimeMillis() - t0
    println(s"scilari;$passes;${dt / 1000.0};1;algorithm=base,faithful=yes")

    // validate
  }

  def validate = {
    val primeCounts: Map[Int, Int] = Map(
      10 -> 4,
      100 -> 25,
      1000 -> 168,
      10000 -> 1229,
      100000 -> 9592,
      1000000 -> 78498,
      10000000 -> 664579,
      100000000 -> 5761455
    )

    val sieve = new Sieve(200)
    sieve.run()
    println(sieve.getPrimes.mkString("Primes up to 200: ", ", ", ""))

    for ((size, expectedCount) <- primeCounts) {
      val sieve = new Sieve(size)
      sieve.run()
      val count = sieve.primeCount
      assert(
        count == expectedCount,
        "Found prime count does not match expected count"
      )
      println(
        s"${sieve.size}: Found prime count ($count) matches with expected ($expectedCount)"
      )
    }
  }

}
