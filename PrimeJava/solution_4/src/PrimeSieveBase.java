import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

public abstract class PrimeSieveBase {
	private static final long TIME = 5000;
	private static final long WARMUP_TIME = 4000;
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
	protected String type = "base";
	protected final int sieveSize;

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
	public abstract void clearBits(int factor);
	
	public void runSieve() {
		int q = (int) Math.sqrt(sieveSize);
		for (int factor = 3; factor <= q; factor += 2) {
			if (getBit(factor)) {
				clearBits(factor);
			}
		}
	}
	
	public void printResults(double duration, int passes, SieveArgs args) {
		if(!args.isWarmup) {
			validateResults();
			System.out.printf("chrvanorle%s%s;%d;%f;%d;algorithm=%s,faithful=yes,bits=%d\n", toString(), args.postfix,passes, duration, args.getThreads(), type, bits);
		}
	}
	
	public String toString() {
		return getClass().getSimpleName().replace("PrimeSieve", "");
	}
	
	static class SieveArgs{
		public String postfix = "";
		public boolean parallel = false;
		public boolean warmup = false;
		public boolean isWarmup = false;
		public int numThreads = Runtime.getRuntime().availableProcessors();
		
		public int getThreads() {
			return parallel ? numThreads : 1;
		}
		
		public long getRuntime() {
			return isWarmup ? WARMUP_TIME : TIME;
		}
	}
	
	public static void run(Supplier<PrimeSieveBase> factory, String[] args) throws InterruptedException {
		SieveArgs a = new SieveArgs();
		for(int i = 0; i < args.length;i++) {
			if(args[i].equals("-postfix") && args.length - 1 > i) {
				a.postfix = args[i + 1];
			}
			if(args[i].equals("-parallel")) {
				a.parallel = true;
			}
			if(args[i].equals("-warmup")) {
				a.warmup = true;
				a.isWarmup = true;
			}
			if(args[i].equals("-threads") && args.length - 1 > i) {
				a.numThreads = Integer.parseInt(args[i + 1]);
			}
		}
		if(a.warmup) runSingle(factory, a);
		a.isWarmup = false;
		if(a.parallel) runParallel(factory, a);
		else runSingle(factory, a);
	}
	
	public static PrimeSieveBase runSingle(Supplier<PrimeSieveBase> factory, SieveArgs args) {
		long runtime = args.getRuntime();
		long start = System.currentTimeMillis();
		int passes = 0;
		PrimeSieveBase sieve = null;
		while ((System.currentTimeMillis() - start) < runtime) {
			sieve = factory.get();
			sieve.runSieve();
			passes++;
		}
		long delta = System.currentTimeMillis() - start;
		if (sieve != null) {
			sieve.printResults(delta / 1000d, passes, args);
		}
		return sieve;
	}
	
	public static void runParallel(Supplier<PrimeSieveBase> factory, SieveArgs args) throws InterruptedException {
		PrimeSieveBase sieve = factory.get();
		sieve.runSieve();// for verification
		List<ParallelSiever> ts = new ArrayList<>();
		AtomicBoolean stop = new AtomicBoolean();
		for(int i = 0; i < args.numThreads;i++) {
			ts.add(new ParallelSiever(factory, stop));
		}
		long start = System.currentTimeMillis();
		ts.forEach(ParallelSiever::start);
		Thread.sleep(args.getRuntime());
		stop.set(true);
		for(ParallelSiever t : ts) {
			t.join();
		}
		long delta = System.currentTimeMillis() - start;
		if (sieve != null) {
			sieve.printResults(delta / 1000d, ts.stream().mapToInt(ParallelSiever::getPasses).sum(), args);
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
}
