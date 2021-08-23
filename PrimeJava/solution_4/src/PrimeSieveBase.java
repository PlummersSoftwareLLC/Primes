import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock.ReadLock;
import java.util.concurrent.locks.ReentrantReadWriteLock.WriteLock;
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
	
	private int getFactor(int factor) {
		for (int num = factor; num <= sieveSize; num += 2)
			if (getBit(num))
				return num;
		
		throw new IllegalStateException("Some shit happened.");
	}
	
	public void runSieve() {
		final double q = Math.sqrt(sieveSize);
		
		for (int factor = 3; factor <= q; factor += 2) {
			factor = getFactor(factor);
						
			for(int num = factor * factor; num <= sieveSize; num += factor << 1)
				clearBit(num);
		}
	}

	public void printResults(double duration, int passes, SieveArgs args) {
		if(args.isWarmup)
			return;
		
		validateResults();
		System.out.printf("chrvanorle%s%s%s;%d;%f;%d;algorithm=%s,faithful=yes,bits=%d\n", toString(), args.warmup ? "W" : "", args.postfix,passes, duration, args.getThreads(), type, bits);
	}
	
	public String toString() {
		return getClass().getSimpleName().replace("PrimeSieve", "");
	}
	static class SieveArgs{
		public String postfix = "";
		public RunModes runMode = RunModes.Sequential;
		public boolean warmup = false;
		public boolean isWarmup = false;
		public int numThreads = Runtime.getRuntime().availableProcessors();
		
		public int getThreads() {
			return runMode == RunModes.MultiThreaded ? numThreads : 1;
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
			if(args[i].equals("-warmup")) {
				a.warmup = true;
				a.isWarmup = true;
			}
			if(args[i].equals("-threads") && args.length - 1 > i) {
				a.runMode = RunModes.MultiThreaded;
				a.numThreads = Integer.parseInt(args[i + 1]);
			}
		}
		
		if(a.warmup)
			RunModes.Sequential.run(factory, a);
		
		a.isWarmup = false;
		
		a.runMode.run(factory, a);
	}
	
	public interface RunMode {
		public void run(Supplier<PrimeSieveBase> factory, SieveArgs args) throws InterruptedException;
	};
	public enum RunModes {
		Sequential		(new SequentialRunMode()),
		MultiThreaded	(new MultiThreadRunMode()),
		;
		
		private final PrimeSieveBase.RunMode rMode;

		RunModes(RunMode m) { this.rMode = m; }
		
		public void run(Supplier<PrimeSieveBase> factory, SieveArgs args) throws InterruptedException {
			rMode.run(factory, args);
		}
	}
	
	public static class SequentialRunMode implements RunMode {
		public void run(Supplier<PrimeSieveBase> factory, SieveArgs args) {
			long runtime = args.getRuntime();
			PrimeSieveBase sieve = factory.get();
			int passes = 0;
			long deadline = System.currentTimeMillis() + runtime;

			do {
				sieve.runSieve();
				passes++;
			} while (System.currentTimeMillis() < deadline);
	
			long delta = System.currentTimeMillis() - (deadline - runtime);
			sieve.printResults(delta / 1000d, passes, args);
		}
	}
	
	public static class MultiThreadRunMode implements RunMode {
		private final ReentrantLock sumLock = new ReentrantLock();
		private int sumOfPasses = 0;
		
		private final ReentrantReadWriteLock canRunLock = new ReentrantReadWriteLock();
		private final ReadLock canRunReadLock = canRunLock.readLock();
		private final WriteLock canRunWriteLock = canRunLock.writeLock();
		private boolean canRun = true;
		
		public void run(final Supplier<PrimeSieveBase> factory, SieveArgs args) throws InterruptedException {
			PrimeSieveBase sieve = factory.get();
			sieve.runSieve();// for verification
			
			List<Thread> ts = new ArrayList<>();
			
			for(int i = 0; i < args.numThreads; i++)
				ts.add(new Thread(new Runnable() {
					private volatile int passes = 0;
					
					@Override
					public void run() {
						final PrimeSieveBase primeSieveRunner = factory.get();
						
						canRunReadLock.lock();
						try {
							for(; canRun; passes++) {
								canRunReadLock.unlock();
								try {
									primeSieveRunner.runSieve();
								} finally {
									canRunReadLock.lock();
								}
							}
						} finally {
							canRunReadLock.unlock();
						}
						
						sumLock.lock();
						try {
							sumOfPasses += passes;
						} finally {
							sumLock.unlock();
						}
					}
				}));
			
			long start = System.currentTimeMillis();
			
			ts.forEach(Thread::start);

			canRunWriteLock.lock();
			try {
				canRunWriteLock.newCondition().awaitNanos(args.getRuntime() * 1000000);
				canRun = false;
			} finally {
				canRunWriteLock.unlock();
			}
			
			for(Thread t : ts)
				t.join();
		
			sieve.printResults((System.currentTimeMillis() - start) / 1000d, sumOfPasses, args);
		}
	}
}
