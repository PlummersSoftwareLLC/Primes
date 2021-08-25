import static java.lang.System.currentTimeMillis;
import static java.util.concurrent.TimeUnit.MILLISECONDS;
import java.util.BitSet;
import java.util.Map;

public class PrimeSieveJavaBitSet {

  private static final int LIMIT = 1000000;
  private static final int TIME = 5;

  // Historical data for validating our results - the number of primes to be found under some
  // limit, such as 168 primes under 1000
  private static final Map<Integer, Integer> MY_DICT = Map.of( //
      10, 4, //
      100, 25, //
      1000, 168, //
      10000, 1229, //
      100000, 9592, //
      1000000, 78498, //
      10000000, 664579, //
      100000000, 5761455 //
  );

  public static void main(final String[] args) {

    var passes = 0;
    PrimeSieve sieve = null;
    final var tStart = currentTimeMillis();

    while (MILLISECONDS.toSeconds(currentTimeMillis() - tStart) < TIME) {
      sieve = new PrimeSieve(LIMIT);
      sieve.runSieve();
      passes++;
    }

    sieve.printResults(false, MILLISECONDS.toSeconds(currentTimeMillis() - tStart), passes);
  }

  public static class PrimeSieve {

    private final int sieveSize;
    private final BitSet bitArray;

    public PrimeSieve(final int size) {
      // Upper limit, highest prime we'll consider
      this.sieveSize = size;
      // since we filter evens, just half as many bits
      final var bitArrayLength = (this.sieveSize + 1) / 2;
      this.bitArray = new BitSet(bitArrayLength);
      this.bitArray.set(0, bitArrayLength, true);
    }

    // Calculate the primes up to the specified limit
    public void runSieve() {
      var factor = 3;
      final var q = (int) Math.sqrt(this.sieveSize);

      while (factor <= q) {
        for (var num = factor; num <= this.sieveSize; num++) {
          if (getBit(num)) {
            factor = num;
            break;
          }
        }

        // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd
        // instance of this factor's multiple. We can then step by factor * 2 because every second
        // one is going to be even by definition
        for (var num = factor * factor; num <= this.sieveSize; num += factor * 2)
          this.clearBit(num);

        factor += 2; // No need to check evens, so skip to next odd (factor = 3, 5, 7, 9...)
      }
    }

    // Return the count of bits that are still set in the sieve. Assumes you've already called
    // runSieve, of course!
    public int countPrimes() {
      return this.bitArray.cardinality();
    }

    // Look up our count of primes in the historical data (if we have it) to see if it matches
    public boolean validateResults() {
      if (MY_DICT.containsKey(this.sieveSize))
        return MY_DICT.get(this.sieveSize) == this.countPrimes();
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

      if (!this.validateResults()) {
        System.out.printf("Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %s%n",
            passes, duration, duration / passes, sieveSize, count, this.validateResults());
        System.out.println();
      }
      System.out.printf("PratimGhosh86-JavaBitSet;%d;%f;1;algorithm=base,faithful=yes,bits=1\n",
          passes, duration);
    }
  }

}
