
public class PrimeSieveJava {
    // Java has a BitSet class included but switching to a boolean array of size
    // improves performance by a lot
    // This brings the limitation of the sieve only being able to test numbers
    // up to Integer.MAX_VALUE - 2 (Requested array size exceeds VM limit)
    private final long[] dataSet;
    private final int sieveSize;

    public static final int BIT_SHIFTS = 7;
    
    
    // Build the set of masks first. We make it 128 entries because every second entry is duplicated
    // to avoid having to bit shift by one since our dataSet doesn't include even numbers
    // I honestly didn't think this would be faster than simply recomputing it on the fly but it is
    public static final long[] masks = new long[128];
    
    static {
        for(int i = 0; i < masks.length; i++) {
            masks[i] = (1L << ((i >> 1) & 0x3F));
        }
    }

    public PrimeSieveJava(int sieveSize) {
        this.sieveSize = sieveSize;
        // unlike the other old implementations the dataSet isn't initialized
        // with true values to optimize speed
        dataSet = new long[((sieveSize) >> BIT_SHIFTS) + 1];
    }

    public int countPrimes() {
        int count = 0;
        for (int i = 0; i < sieveSize; i++) {
            if (getBit(i)) {
                count++;
            }
        }

        return count;
    }

    public boolean validateResults() {
        return lookupValidationData(sieveSize) == countPrimes();
    }

    // Naming hasn't changed for comparison of the methods but removing the if
    // statement for a branchless comparison
    // and inverting the statement due to not initializing the dataSet to true
    // results in a boost too.
    // Also rather interesting: checking index % 2 != 0 is slower than index % 2
    // == 1
    private boolean getBit(int index) {
        return (index & 1) == 1 && (dataSet[index >> BIT_SHIFTS] & masks[(index & 0x7F)]) == 0;
    }

    // Again instead of checking if index is even we just update the array at
    // that index equivalent to that check
    // to boost performance
    private void clearBit(int index) {
        dataSet[index >> BIT_SHIFTS] |= masks[(index & 0x7F)];
    }

    public void runSieve() {
        int factor = 3;
        int q = (int) Math.sqrt(sieveSize);

        while (factor <= q) {
            for (int num = factor; num <= sieveSize; num++) {
                if (getBit(num)) {
                    factor = num;
                    break;
                }
            }
            
            for (int num = factor * factor; num <= sieveSize; num += factor * 2) {
                clearBit(num);
            }

            factor += 2;
        }
    }

    public void printResults(boolean showResults, double duration, int passes) {
        if (showResults) {
            System.out.print("2, ");
        }

        int count = 1;
        for (int num = 3; num <= this.sieveSize; num++) {
            if (getBit(num)) {
                if (showResults) {
                    System.out.print(num + ", ");
                }

                count++;
            }
        }

        if (showResults) {
            System.out.println();
        }

        System.out.printf("Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %s%n",
                          passes,
                          duration,
                          (duration / passes),
                          sieveSize,
                          count,
                          validateResults());

        // Following 2 lines added by rbergen to conform to drag race output
        // format
        System.out.println();
        System.out.printf("AndrewT;%d;%f;1;algorithm=base,faithful=yes\n", passes, duration);

    }

    public static void main(String[] args) {
        long start = System.currentTimeMillis();
        int passes = 0;
        PrimeSieveJava sieve = null;

        while ((System.currentTimeMillis() - start) < 5000) {
            sieve = new PrimeSieveJava(1000000);
            sieve.runSieve();
            passes++;
        }

        long delta = System.currentTimeMillis() - start;
        if (sieve != null) {
            sieve.printResults(true, delta / 1000d, passes);
        }
    }

    private static int lookupValidationData(int key) {
        switch (key) {
            case 10:
                return 4;
            case 100:
                return 25;
            case 1000:
                return 168;
            case 10000:
                return 1229;
            case 100000:
                return 9592;
            case 1000000:
                return 78498;
            case 10000000:
                return 664579;
            case 100000000:
                return 5761455;
            default:
                return -1;
        }
    }
}
