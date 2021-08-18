import scala.collection.mutable.BitSet
import scala.math.{sqrt, floor}

object PrimeScala:
  private val resultsDictionary = Map(
               10 -> 4,
              100 -> 25,
            1_000 -> 168,
           10_000 -> 1_229,
          100_000 -> 9_592,
        1_000_000 -> 78_498,
       10_000_000 -> 664_579,
      100_000_000 -> 5_761_455,
    1_000_000_000 -> 50_847_534)

class PrimeScala(sieveSize: Int):
  private val bits = BitSet((sieveSize + 1) >> 1)
  
  private def validateResults: Boolean = 
    PrimeScala.resultsDictionary(sieveSize) == countPrimes
  
  private def getBit(index: Int): Boolean = 
    !bits(index >> 1)
  
  private def clearBit(index: Int): Unit = 
    bits(index >> 1) = true
  
  def runSieve(): Unit = 
    var factor = 3
    val q = floor(sqrt(sieveSize))
    
    while factor <= q do
      factor = (factor until sieveSize by 2)
        .find(getBit)
        .getOrElse(factor)

      for
        num <- factor * factor until sieveSize by factor * 2
      do
        clearBit(num)

      factor += 2
  
  def printResults(showResults: Boolean, duration: Double, passes: Int) = 
    if showResults then printf("2, ")
    
    var count = if sieveSize >= 2 then 1 else 0

    for
      num <- 3 until sieveSize by 2
      if getBit(num)
    do
      if showResults then 
        printf("%d, ", num)
      count += 1

    if showResults then
      printf("\n")
      printf("Passes: %d, Time: %f, ", passes, duration)
      printf("Avg: %f, Limit: %d, ", duration / passes, sieveSize)
      printf("Count1: %d, Count2: %d, ", count, countPrimes)
      printf("Valid: %s\n", if validateResults then "True" else "False")
      printf("\n")

    printf("mmcdon20_scala;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", 
        passes, duration)
  
  private def countPrimes: Int = 
    (0 until ((sieveSize + 1) >> 1)).count(!bits(_))

@main def main =
  var passes = 0
  val start = System.currentTimeMillis
  var continue = true

  while continue do
    val sieve = new PrimeScala(1_000_000)
    sieve.runSieve()
    passes += 1
    val stop = System.currentTimeMillis

    if stop - start >= 5_000 then
      sieve.printResults(false, (stop - start) / 1_000.0, passes)
      continue = false
