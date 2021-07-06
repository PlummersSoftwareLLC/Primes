/*
 * Java Prime Sieve
 */

import static java.lang.System.currentTimeMillis;
import static java.util.concurrent.TimeUnit.MILLISECONDS;
import java.util.BitSet;
import java.util.Map;

public class PrimeSieveJavaBitSet {

  public static void main(final String[] args) {

    long passes = 0;
    PrimeSieve sieve = null;
    final long tStart = currentTimeMillis();

    while (MILLISECONDS.toSeconds(currentTimeMillis() - tStart) < 5) {
      sieve = new PrimeSieve(1000000);
      sieve.runSieve();
      passes++;
    }

    final long tD = currentTimeMillis() - tStart;
    if (sieve != null)
      sieve.printResults(false, MILLISECONDS.toSeconds(tD), passes);
  }

  public static class PrimeSieve {

    private int sieveSize = 0; // Storage for sieve - since we filter evens, just half as many bits
    private final BitSet bitArray; // Upper limit, highest prime we'll consider

    // Historical data for validating our results - the number of primes to be found under some
    // limit, such as 168 primes under 1000
    private final Map<Integer, Integer> myDict = Map.of( //
        10, 1, //
        1000, 168, //
        10000, 1229, //
        100000, 9592, //
        1000000, 78498, //
        10000000, 664579, //
        100000000, 5761455 //
    );

    public PrimeSieve(final int size) {
      this.sieveSize = size;
      final var bitArrayLength = (this.sieveSize + 1) / 2;
      this.bitArray = new BitSet(bitArrayLength);
      this.bitArray.set(0, bitArrayLength, true);
    }

    // Return the count of bits that are still set in the sieve. Assumes you've already called
    // runSieve, of course!
    public int countPrimes() {
      return this.bitArray.cardinality();
    }

    // Look up our count of primes in the historical data (if we have it) to see if it matches
    public boolean validateResults() {
      if (this.myDict.containsKey(this.sieveSize))
        return this.myDict.get(this.sieveSize) == this.countPrimes();
      return false;
    }

    // Gets a bit from the array of bits, but automatically just filters out even numbers as
    // false, and then only uses half as many bits for actual storage
    private boolean getBit(final int index) {
      if (index % 2 == 0)
        return false;
      return this.bitArray.get(index / 2);
    }

    // Reciprocal of GetBit, ignores even numbers and just stores the odds. Since the prime sieve
    // work should never waste time clearing even numbers, this code will assert if you try to
    private void clearBit(final int index) {
      if (index % 2 == 0) {
        System.out.println("You are setting even bits, which is sub-optimal");
      }
      this.bitArray.set(index / 2, false);
    }

    // Calculate the primes up to the specified limit
    public void runSieve() {
      var factor = 3;
      final var q = (int) Math.sqrt(this.sieveSize);

      while (factor < q) {
        for (var num = factor; num <= this.sieveSize; num++) {
          if (getBit(num)) {
            factor = num;
            break;
          }
        }

        // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd
        // instance of this factor's multiple. We can then step by factor * 2 because every second
        // one is going to be even by definition
        for (var num = factor * 3; num <= this.sieveSize; num += factor * 2)
          this.clearBit(num);

        factor += 2; // No need to check evens, so skip to next odd (factor = 3, 5, 7, 9...)
      }
    }

    // Displays the primes found (or just the total count, depending on what you ask for)
    public void printResults(final boolean showResults, final double duration, final long passes) {
      if (showResults)
        System.out.print("2, ");

      var count = 1;
      for (var num = 3; num <= this.sieveSize; num++) {
        if (this.getBit(num)) {
          if (showResults)
            System.out.printf("%d, ", String.valueOf(num));
          count++;
        }
      }
      if (showResults)
        System.out.println("");

      System.out.printf("Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %s%n", passes,
          duration, duration / passes, sieveSize, count, this.validateResults());
      System.out.println();
      System.out.printf("PratimGhosh86;%d;%f;1;algorithm=base,faithful=yes\n", passes, duration);
    }
  }

}
