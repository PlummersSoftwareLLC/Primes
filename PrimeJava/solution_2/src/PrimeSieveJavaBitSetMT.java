import java.util.BitSet;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAccumulator;

public class PrimeSieveJavaBitSetMT {

  // upper limit, highest prime we'll consider
  private static final int SIEVE_SIZE = 1000000;

  // for how long the sieve calculation will run
  private static final int TIME_IN_SECONDS = 5;

  // used to ensure that no waiting thread runs after the time limit is reached
  private static final AtomicBoolean SHOULD_RUN = new AtomicBoolean(true);

  // to store number of times the sieve calculation completed successfully
  private static final AtomicLong PASSES = new AtomicLong(0L);

  // accumulator to store count of primes from individual thread
  // if each run produced a correct value, it should match count
  // from dictionary when divided by the number of passes
  private static final LongAccumulator COUNT_OF_PRIMES_ACCUMULATOR =
      new LongAccumulator(Long::sum, 0L);

  // cached thread pool to spawn as many threads as possible
  // alternatively, Executors.newFixedThreadPool(n) can be used to spawn a fixed number of thread
  private static final ThreadPoolExecutor POOL =
      (ThreadPoolExecutor) Executors.newCachedThreadPool();

  public static void main(final String[] args) {
    POOL.setCorePoolSize(Runtime.getRuntime().availableProcessors());
    final var tStart = System.currentTimeMillis();
    while (TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis() - tStart) < TIME_IN_SECONDS) {
      POOL.execute(() -> runSieve(new PrimeSieve(SIEVE_SIZE)));
    }
    SHOULD_RUN.set(false);
    shutdownPool();
    final var tEnd = System.currentTimeMillis();
    printResults(SIEVE_SIZE, PASSES.get(), TimeUnit.MILLISECONDS.toSeconds(tEnd - tStart),
        COUNT_OF_PRIMES_ACCUMULATOR.longValue(), Runtime.getRuntime().availableProcessors());
  }

  // Calculate the primes up to the specified limit
  public static void runSieve(final PrimeSieve sieve) {
    // make sure any waiting threads are skipped in time limit is reached
    if (!SHOULD_RUN.get())
      return;
    final var sieveSize = sieve.getSieveSize();
    final var bitSet = sieve.getBitArray();
    var factor = 3;
    final var q = (int) Math.sqrt(sieveSize);
    while (factor <= q) {
      for (var num = factor; num <= sieveSize; num++) {
        if (getBit(bitSet, num)) {
          factor = num;
          break;
        }
      }
      // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd
      // instance of this factor's multiple. We can then step by factor * 2 because every second
      // one is going to be even by definition
      for (var num = factor * factor; num <= sieveSize; num += factor * 2)
        clearBit(bitSet, num);
      factor += 2; // No need to check evens, so skip to next odd (factor = 3, 5, 7, 9...)
    }
    if (SHOULD_RUN.get())
      validateAndStoreResults(sieveSize, countPrimes(bitSet));
  }

  // Gets a bit from the array of bits, but automatically just filters out even numbers as
  // false, and then only uses half as many bits for actual storage
  private static boolean getBit(final BitSet bitSet, final int index) {
    if (index % 2 == 0)
      return false;
    return bitSet.get(index / 2);
  }

  // Reciprocal of GetBit, ignores even numbers and just stores the odds. Since the prime sieve
  // work should never waste time clearing even numbers, this code will assert if you try to
  private static void clearBit(final BitSet bitSet, final int index) {
    if (index % 2 == 0) {
      System.out.println("You are setting even bits, which is sub-optimal");
    }
    bitSet.set(index / 2, false);
  }

  // Return the count of bits that are still set in the sieve. Assumes you've already called
  // runSieve, of course!
  public static int countPrimes(final BitSet bitArray) {
    return bitArray.cardinality();
  }

  // Look up our count of primes in the historical data (if we have it) to see if it matches
  // If a match is found then store the sieve count and increment the number of passes
  private static void validateAndStoreResults(final int sieveSize, final int countOfPrimes) {
    if (MY_DICT.containsKey(sieveSize) && MY_DICT.get(sieveSize) == countOfPrimes) {
      COUNT_OF_PRIMES_ACCUMULATOR.accumulate(countOfPrimes);
      PASSES.incrementAndGet();
    }
  }

  // Look up our count of primes in the historical data (if we have it) to see if it matches
  private static boolean validate(final int sieveSize, final long count) {
    if (MY_DICT.containsKey(sieveSize))
      return MY_DICT.get(sieveSize) == count;
    return false;
  }

  private static void printResults(final int sieveSize, final long passes,
      final double durationInSeconds, final long accumulatedCount, final int maxThreadCount) {
    if (!validate(sieveSize, accumulatedCount / passes)) {
      System.out.printf("Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %s%n", passes,
          durationInSeconds, durationInSeconds / passes, sieveSize, accumulatedCount / passes,
          validate(sieveSize, accumulatedCount / passes));
      System.out.println();
    }
    System.out.printf("PratimGhosh86-JavaBitSetMT;%d;%f;%d;algorithm=base,faithful=yes,bits=1\n",
        passes, durationInSeconds, maxThreadCount);
  }

  private static void shutdownPool() {
    try {
      // Disable new tasks from being submitted
      POOL.shutdown();
      // Wait a while for existing tasks to terminate
      POOL.awaitTermination(1, TimeUnit.MILLISECONDS);
    } catch (final InterruptedException e) {
      // Preserve interrupt status
      Thread.currentThread().interrupt();
    } finally {
      POOL.shutdownNow(); // Cancel currently executing tasks
    }
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

    public int getSieveSize() {
      return sieveSize;
    }

    public BitSet getBitArray() {
      return bitArray;
    }

  }

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

}
