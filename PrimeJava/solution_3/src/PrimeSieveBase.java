import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

public abstract class PrimeSieveBase {
	private static final long TIME = 5000;
	private static final Map<Integer, Integer> VALIDATION_DATA;

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
	
	protected int bits = 1;
	protected int sieveSize = 0;

	public PrimeSieveBase(int size) {
		sieveSize = size;
	}

	public boolean validateResults() {
		boolean result = false;
		if (VALIDATION_DATA.containsKey(this.sieveSize)) {
			result = VALIDATION_DATA.get(this.sieveSize) == this.countPrimes();
		}
		if(!result) System.err.println("Invalid, expected "+VALIDATION_DATA.get(sieveSize)+" but got "+countPrimes());
		return result;
	}
	
	public int countPrimes() {
		int c = 0;
		for(int i = 1; i <= sieveSize;i += 2) {
			if(getBit(i)) c++;
		}
		return c;
	}
	
	public abstract boolean getBit(int index);
	public abstract void clearBit(int index);
	
	public void runSieve() {
		int q = (int) Math.sqrt(sieveSize);
		for (int factor = 3; factor <= q; factor += 2) {
			for (int num = factor; num <= sieveSize; num += 2) {
				if (getBit(num)) {
					factor = num;
					break;
				}
			}
			for (int num = factor * factor; num <= sieveSize; num += factor * 2) {
				clearBit(num);
			}
		}
	}

	public void printResults(boolean showResults, double duration, int passes, int threads) {
		if (showResults) {
			System.out.print("2, ");
		}
		int count = 1;
		for (int num = 3; num <= this.sieveSize; num++) {
			if ((num & 1) == 1 && getBit(num)) {
				if (showResults) {
					System.out.print(num + ", ");
				}
				count++;
			}
		}
		if (showResults) {
			System.out.println();
		}
		System.out.printf("Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %s%n", passes, duration, (duration / passes), sieveSize, count, validateResults());
		System.out.println();
		System.out.printf("chrvanorle%s;%d;%f;%d;algorithm=base,faithful=yes,bits=%d\n", toString(), passes, duration, threads, bits);
	}
	
	public String toString() {
		return getClass().getSimpleName().replace("PrimeSieve", "");
	}
	
	public static void run(Supplier<PrimeSieveBase> factory, String[] args) throws InterruptedException {
		boolean parallel = false;
		int numThreads = Runtime.getRuntime().availableProcessors();
		for(int i = 0; i < args.length;i++) {
			if(args[i].equals("-parallel")) {
				parallel = true;
			}
			if(args[i].equals("-threads") && args.length - 1 > i) {
				numThreads = Integer.parseInt(args[i + 1]);
			}
		}
		if(parallel) runParallel(factory, numThreads);
		else runSingle(factory);
	}
	
	public static void runSingle(Supplier<PrimeSieveBase> factory) {
		long start = System.currentTimeMillis();
		int passes = 0;
		PrimeSieveBase sieve = null;
		while ((System.currentTimeMillis() - start) < TIME) {
			sieve = factory.get();
			sieve.runSieve();
			passes++;
		}
		long delta = System.currentTimeMillis() - start;
		if (sieve != null) {
			sieve.printResults(false, delta / 1000d, passes, 1);
		}
	}
	
	static class ParallelSiever implements Runnable{
		private Thread t;
		private int passes;
		private AtomicBoolean stop;
		private Supplier<PrimeSieveBase> factory;
		public ParallelSiever(Supplier<PrimeSieveBase> factory, AtomicBoolean stop) {
			this.stop = stop;
			this.factory = factory;
			this.t = new Thread(this);
		}
		
		public int getPasses() {
			return passes;
		}
		
		public void start() {
			t.start();
		}
		
		public void join() throws InterruptedException {
			t.join();
		}
		
		public void run() {
			PrimeSieveBase sieve = null;
			while(!stop.get()) {
				sieve = factory.get();
				sieve.runSieve();
				passes++;
			}
		}
	}
	
	public static void runParallel(Supplier<PrimeSieveBase> factory, int numThreads) throws InterruptedException {
		PrimeSieveBase sieve = factory.get();
		sieve.runSieve();// for verification
		List<ParallelSiever> ts = new ArrayList<>();
		AtomicBoolean stop = new AtomicBoolean();
		for(int i = 0; i < numThreads;i++) {
			ts.add(new ParallelSiever(factory, stop));
		}
		long start = System.currentTimeMillis();
		ts.forEach(ParallelSiever::start);
		Thread.sleep(TIME);
		stop.set(true);
		for(ParallelSiever t : ts) {
			t.join();
		}
		long delta = System.currentTimeMillis() - start;
		if (sieve != null) {
			sieve.printResults(false, delta / 1000d, ts.stream().mapToInt(ParallelSiever::getPasses).sum(), numThreads);
		}
	}
}
