import java.util.HashMap;
import java.util.Map;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Objects;

public class PrimeSieveJava {
	private static final Map<Integer, Integer> VALIDATION_DATA;
	private final boolean[] sieveArray;
	private final BitSet sieveSet;
	private final int n;
	private final String variant;

	public PrimeSieveJava(int n, String variant) {
		this.variant = variant;
		this.n = n;
		int half_n = (n + 1) >> 1;
		if (Objects.equals(variant, "bitset")) {
			sieveArray = null;
			sieveSet = new BitSet(half_n);
			sieveSet.set(0, half_n);
		} else {
			sieveSet = null;
			sieveArray = new boolean[half_n];
		}
	}

	public int count() {
		if (Objects.equals(variant, "bitset")) {
			return this.sieveSet.cardinality();
		} else {
			int count = 0;
			for (int i = 0; i < sieveArray.length; i++) {
				if (!sieveArray[i]) {
					count++;
				}
			}
			return count;
		}
	}

	public boolean validateResults() {
		if (VALIDATION_DATA.containsKey(n)) {
			return VALIDATION_DATA.get(n) == count();
		}
		return false;
	}

	public void runArray() {
		int q = (int) Math.ceil(Math.sqrt(n));
		for (int p = 3; p < q; p += 2) {
			if (!sieveArray[p >> 1]) {
				for (int i = (p * p) >> 1; i < n >> 1; i += p) {
					sieveArray[i] = true;
				}
			}
		}
	}

	public void runBitSet() {
		int q = (int) Math.ceil(Math.sqrt(n));
		for (int p = 3; p < q; p += 2) {
			if (sieveSet.get(p >> 1)) {
				for (int i = (p * p) >> 1; i < n >> 1; i += p) {
					sieveSet.clear(i);
				}
			}
		}
	}

	public void printResults(double duration, int passes) {
		int count = count();
		String label = Objects.equals(variant, "bitset") ? "pez-bitset" : "MansenC+pez-boolean-array";
		String bits = Objects.equals(variant, "bitset") ? "1" : "8";
		System.out.printf("Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %s%n", passes,
				duration, (duration / passes), n, count, validateResults());
		System.out.printf("%s;%d;%f;1;algorithm=base,faithful=yes,bits=%s\n", label, passes, duration, bits);
	}

	public static void main(String[] args) {
		long start = System.currentTimeMillis();
		PrimeSieveJava sieve = null;
		int limit = 1000000;
		boolean warmup = false;
		int warmupTime = 5000;
		int runTime = 5000;
		String variant = "array";

		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-limit") && args.length - 1 > i) {
				limit = Integer.parseInt(args[i + 1]);
			}
			if (args[i].equals("-variant") && args.length - 1 > i) {
				variant = args[i + 1];
			}
			if (args[i].equals("-warmup")) {
				warmup = true;
			}
		}

		if (warmup) {
			if (Objects.equals(variant, "bitset")) {
				while ((System.currentTimeMillis() - start) < warmupTime) {
					sieve = new PrimeSieveJava(limit, variant);
					sieve.runBitSet();
				}
			} else {
				while ((System.currentTimeMillis() - start) < warmupTime) {
					sieve = new PrimeSieveJava(limit, variant);
					sieve.runArray();
				}
			}
		}

		int passes = 0;
		start = System.currentTimeMillis();
		if (Objects.equals(variant, "bitset")) {
			while ((System.currentTimeMillis() - start) < runTime) {
				sieve = new PrimeSieveJava(limit, variant);
				sieve.runBitSet();
				passes++;
			}
		} else {
			while ((System.currentTimeMillis() - start) < runTime) {
				sieve = new PrimeSieveJava(limit, variant);
				sieve.runArray();
				passes++;
			}
		}

		long delta = System.currentTimeMillis() - start;
		if (sieve != null) {
			sieve.printResults(delta / 1000d, passes);
		}
	}

	static {
		VALIDATION_DATA = new HashMap<>();
		VALIDATION_DATA.put(10, 4);
		VALIDATION_DATA.put(100, 25);
		VALIDATION_DATA.put(1000, 168);
		VALIDATION_DATA.put(10000, 1229);
		VALIDATION_DATA.put(100000, 9592);
		VALIDATION_DATA.put(1000000, 78498);
		VALIDATION_DATA.put(10000000, 664579);
		VALIDATION_DATA.put(100000000, 5761455);
	}
}
