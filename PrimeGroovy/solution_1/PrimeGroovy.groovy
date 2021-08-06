import java.util.BitSet

class PrimeGroovy {
    private int sieveSize
    private BitSet bits
    private static def resultsDictionary = [
        10: 4,
        100: 25,
        1000: 168,
        10000: 1229,
        100000: 9592,
        1000000: 78498,
        10000000: 664579,
        100000000: 5761455,
        1000000000: 50847534,
        10000000000: 455052511 
    ]

    PrimeGroovy(int size) {
        this.sieveSize = size
        this.bits = new BitSet((size + 1) >> 1)
        bits.flip(0, (sieveSize + 1) >> 1)
    }

    private boolean validateResults() {
        return resultsDictionary[sieveSize] == countPrimes()
    }

    private boolean getBit(int index) {
        return bits.get(index >> 1)
    }

    private void clearBit(int index) {
        bits.clear(index >> 1)
    }

    void runSieve() {
        int factor = 3
        int q = (int) Math.sqrt((double) sieveSize)

        while (factor <= q) {
            for (var num = factor; num < sieveSize; num += 2) {
                if (getBit(num)) {
                    factor = num
                    break
                }
            }

            for (var num = factor * factor; num < sieveSize; num += factor * 2) {
                clearBit(num)
            }

            factor += 2
        }
    }

    void printResults(boolean showResults, double duration, int passes) {
        if (showResults) {
            printf("2, ")
        }

        int count = (sieveSize >= 2) ? 1 : 0

        for (var num = 3; num <= sieveSize; num += 2) {
            if (getBit(num)) {
                if (showResults) {
                    printf("%d, ", num)
                }
                count++
            }
        }

        if (showResults) {
            printf("\n")
            printf("Passes: %d, Time: %f, ", passes, duration)
            printf("Avg: %f, Limit: %d, ", duration / passes, sieveSize)
            printf("Count1: %d, Count2: %d, ", count, countPrimes())
            printf("Valid: %s\n", validateResults() ? "True" : "False")
            printf("\n")
        }

        printf("mmcdon20_groovy;%d;%f;1;algorithm=base,faithful=yes,bits=1\n",
            passes, duration)
    }

    int countPrimes() {
        return this.bits.cardinality()
    }

    static void main(String... args) {
        int passes = 0
        long start = System.currentTimeMillis()

        while (true) {
            PrimeGroovy sieve = new PrimeGroovy(1000000)
            sieve.runSieve()
            passes++
            long stop = System.currentTimeMillis()

            if (stop - start >= 5000) {
                sieve.printResults(false, (stop - start) / 1000.0, passes)
                break
            }
        }
    }
}
