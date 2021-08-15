import Math.sqrt

class PrimeSieve(sieveSize: Int) {
  val rawBits: Array[Boolean] = Array.fill((sieveSize + 1) / 2)(true)

  val primeCounts: Map[Int, Int] = Map(
    10        -> 4, //Historical data for validating our results - the number of primes
    100       -> 25, //to be found under some limit, such as 168 primes under 1000
    1000      -> 168,
    10000     -> 1229,
    100000    -> 9592,
    1000000   -> 78498,
    10000000  -> 664579,
    100000000 -> 5761455
  )

  def validateResults(): Boolean =
    //Look up our count of primes in the historical data (if we have it) to see if it matches
    primeCounts.get(sieveSize) match {
      case Some(value) if value == countPrimes() => true
      case _                                     => false
    }

  @inline def getBit(index: Int): Boolean =
    //Gets a bit from the array of bits, but automatically just filters out even numbers as false, and then only uses half as many bits for actual storage
    if (index % 2 == 0) false else rawBits(index / 2)

  @inline def clearBit(index: Int): Unit =
    //Reciprocal of GetBit, ignores even numbers and just stores the odds. Since the prime sieve work should never waste time clearing even numbers, this code will assert if you try to
    rawBits(index / 2) = false

  def runSieve(): Unit = {
    var factor = 3
    val q      = sqrt(sieveSize)

    while (factor < q) {
      (factor until sieveSize by 2).iterator
        .dropWhile(i => !rawBits(i / 2))
        .nextOption() match {
        case Some(value) => factor = value
        case _           =>
      }

      for (num <- factor * 3 until sieveSize by factor * 2)
        // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
        // We can then step by factor * 2 because every second one is going to be even by definition
        clearBit(num)

      factor += 2 //No need to check evens, so skip to next odd (factor = 3, 5, 7, 9...)
    }
  }

  def countPrimes(): Int =
    //Return the count of bits that are still set in the sieve. Assumes you've already called runSieve, of course!
    rawBits.count(_ == true)

  def printResults(showResults: Boolean, duration: Double, passes: Int): Unit = {
    //    Displays the primes found (or just the total count, depending on what you ask for)
    if (showResults) // Since we auto-filter evens, we have to special case the number 2 which is prime
      print("2, ")

    var count = 1
    for (num <- 3 until sieveSize) { // Count (and optionally dump) the primes that were found below the limit
      if (getBit(num)) {
        if (showResults)
          print(f"$num, ")
        count += 1
      }
    }

    if (showResults) println()

    println(s"rom1dep;$passes;$duration;1;algorithm=base,faithful=yes")
  }
}

object Sieve extends App {
  val tStart = System.currentTimeMillis // Record our starting time
  // We're going to count how many passes we make in fixed window of time
  var passes = 0

  var sieve: PrimeSieve = null
  while (System.currentTimeMillis() - tStart < 5000) {  // Run until more than 5 seconds have elapsed
    sieve = new PrimeSieve(1000000)                     //  Calc the primes up to a million
    sieve.runSieve()                                    //  Find the results
    passes += 1                                         //  Count this pass
  }

  // After the "at least 5 seconds", get the actual elapsed
  val tD = System.currentTimeMillis() - tStart

  sieve.printResults(false, tD/1000d, passes) // Display outcome

}
