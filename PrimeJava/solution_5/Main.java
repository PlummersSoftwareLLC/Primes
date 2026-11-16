import java.util.Locale;
import java.util.Random;

public final class Main {
    private static final int SIEVE_SIZE = 1_000_000;
    private static final int EXPECTED_COUNT = 78_498;
    private static final int DEFAULT_SEGMENT = 32 * 1024;
    // With a bounds check on every byte the "extreme" loop costs more, so more factors
    // are worth a vector pattern.
    private static final int DEFAULT_VECTOR_MAX = 191;
    private static final int DEFAULT_VECTOR_MAX_UNCHECKED = 95;

    public static void main(String[] args) {
        // For experiments. The defaults are what the benchmark runs.
        boolean unchecked = false;
        boolean test = false;
        int segment = DEFAULT_SEGMENT;
        int vectorMax = -1;
        double warmup = 2.0;
        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
                case "--unsafe" -> unchecked = true;
                case "--test" -> test = true;
                case "--segment" -> segment = Integer.parseInt(args[++i]);
                case "--vecmax" -> vectorMax = Integer.parseInt(args[++i]);
                case "--warmup" -> warmup = Double.parseDouble(args[++i]);
                default -> throw new IllegalArgumentException(args[i]);
            }
        }
        int defaultVectorMax = unchecked ? DEFAULT_VECTOR_MAX_UNCHECKED : DEFAULT_VECTOR_MAX;
        if (vectorMax < 0) {
            vectorMax = defaultVectorMax;
        }
        if (test) {
            test(vectorMax, unchecked);
            return;
        }

        // A non-default setting gets its own label, so results can't be mixed up.
        String label = unchecked ? "pbednarcik-java-unsafe" : "pbednarcik-java";
        if (segment != DEFAULT_SEGMENT) {
            label += "-seg" + segment / 1024 + "k";
        }
        if (vectorMax != defaultVectorMax) {
            label += "-vec" + vectorMax;
        }

        // Untimed passes first, so the JIT is finished before the clock starts.
        long warmupEnd = System.nanoTime() + (long) (warmup * 1e9);
        while (System.nanoTime() < warmupEnd) {
            new PrimeSieve(SIEVE_SIZE, vectorMax, unchecked).run(segment);
        }

        // The timed run: a new sieve every pass, for 5 seconds.
        PrimeSieve sieve = null;
        int passes = 0;
        long started = System.nanoTime();
        do {
            sieve = new PrimeSieve(SIEVE_SIZE, vectorMax, unchecked);
            sieve.run(segment);
            passes++;
        } while (System.nanoTime() - started < 5_000_000_000L);
        double seconds = (System.nanoTime() - started) / 1e9;

        int count = sieve.countPrimes();
        if (count != EXPECTED_COUNT) {
            System.err.printf("Invalid result: counted %d primes up to %d, expected %d.%n",
                    count, SIEVE_SIZE, EXPECTED_COUNT);
            System.exit(1);
        }
        System.out.printf(Locale.ROOT, "%s;%d;%f;1;algorithm=base,faithful=yes,bits=1%n", label, passes, seconds);
    }

    // Compares the sieve with a plain one for every size up to 3,000 and 100 random
    // sizes. With tiny segments nearly every loop gets cut by a segment boundary.
    private static void test(int vectorMax, boolean unchecked) {
        int max = 3_000_000;
        boolean[] composite = new boolean[max + 1];
        for (int f = 2; f * f <= max; f++) {
            if (!composite[f]) {
                for (int m = f * f; m <= max; m += f) {
                    composite[m] = true;
                }
            }
        }
        Random random = new Random(20261116);
        int sieves = 0;
        for (int segment : new int[] {8, 64, 1_000, 16_384, DEFAULT_SEGMENT}) {
            for (int k = 0; k <= 3_100; k++) {
                int size = k <= 3_000 ? k : 3_000 + random.nextInt(max - 3_000 + 1);
                PrimeSieve sieve = new PrimeSieve(size, vectorMax, unchecked);
                sieve.run(segment);
                int expected = 0;
                for (int n = 0; n <= size; n++) {
                    boolean prime = n >= 2 && !composite[n];
                    if (prime) {
                        expected++;
                    }
                    if (sieve.isPrime(n) != prime) {
                        throw new AssertionError("size " + size + ", segment " + segment + ": wrong at " + n);
                    }
                }
                if (sieve.countPrimes() != expected) {
                    throw new AssertionError("size " + size + ", segment " + segment + ": wrong count");
                }
                sieves++;
            }
        }
        System.out.println("OK: " + sieves + " sieves compared with a plain sieve.");
    }
}
