import java.util.HashMap;
import java.util.Map;
import java.util.Arrays;

public class Sieve {
	private static final Map<Integer, Integer> VALIDATION_DATA;
	
	private final boolean[] dataSet;
	private final int sieveSize;
	
	public Sieve(int sieveSize) {
		this.sieveSize = sieveSize;
		dataSet = new boolean[(sieveSize + 1) >> 1];
		Arrays.fill(dataSet, true);
	}
	
	public int count() {
		int count = 0;
		for (int i = 0; i < dataSet.length; i++) {
			if (dataSet[i]) {
				count++;
			}
		}
		return count;
	}
	
	public boolean validateResults() {
		if (VALIDATION_DATA.containsKey(sieveSize)) {
			return VALIDATION_DATA.get(sieveSize) == count();
		}
		return false;
	}
	
	public void run() {
		int q = (int) Math.ceil(Math.sqrt(sieveSize));
		for (int p = 3; p < q; p += 2) {
			if (dataSet[p >> 1]) {
				for (int i = (p * p) >> 1; i < sieveSize >> 1; i += p) {
					dataSet[i] = false;
				}
			}			
		}
	}
	
	public void printResults(double duration, int passes) {		
		int count = count();		
		System.out.printf("Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %s%n", passes, duration, (duration / passes), sieveSize, count, validateResults());
		System.out.printf("pez;%d;%f;1;algorithm=base,faithful=yes,bits=8\n", passes, duration);
	}
	
	public static void main(String[] args) {
		long start = System.currentTimeMillis();
		Sieve sieve = null;
		int limit = 1000000;
		boolean warmup = false;
		int warmupTime = 5000;
		int runTime = 5000;

		for(int i = 0; i < args.length;i++) {
			if(args[i].equals("-limit") && args.length - 1 > i) {
				limit = Integer.parseInt(args[i + 1]);
			}
			if(args[i].equals("-warmup")) {
				warmup = true;
			}
		}

		if (warmup) {
			while ((System.currentTimeMillis() - start) < warmupTime) {
				sieve = new Sieve(limit);
				sieve.run();
			}
		}

		int passes = 0;
		start = System.currentTimeMillis();		
		while ((System.currentTimeMillis() - start) < runTime) {
			sieve = new Sieve(limit);
			sieve.run();
			passes++;
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
