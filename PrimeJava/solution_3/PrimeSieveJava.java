import java.util.HashMap;
import java.util.Map;

/** Note this file was copied from ../solution_1 */
public class PrimeSieveJava
{
	private static final Map<Integer, Integer> VALIDATION_DATA;
	
	// Java has a BitSet class included but switching to a boolean array of size improves performance by a lot
	// This brings the limitation of the sieve only being able to test numbers up to Integer.MAX_VALUE - 2 (Requested array size exceeds VM limit)
	private final boolean[] dataSet;
	private final int sieveSize;
	
	public PrimeSieveJava(int sieveSize)
	{
		this.sieveSize = sieveSize;
		// unlike the other old implementations the dataSet isn't initialized with true values to optimize speed
		dataSet = new boolean[(sieveSize + 1) >> 1];
	}
	
	public int countPrimes()
	{
		int count = 0;
		for (int i = 0; i < dataSet.length; i++)
		{
			if (!dataSet[i])
			{
				count++;
			}
		}
		
		return count;
	}
	
	public boolean validateResults()
	{
		if (VALIDATION_DATA.containsKey(sieveSize))
		{
			return VALIDATION_DATA.get(sieveSize) == countPrimes();
		}
		
		return false;
	}
	
	// Naming hasn't changed for comparison of the methods but removing the if statement for a branchless comparison
	// and inverting the statement due to not initializing the dataSet to true results in a boost too.
	// Also rather interesting: checking index % 2 != 0 is slower than index % 2 == 1
	private boolean getBit(int index)
	{
		return (index & 1) == 1 && !dataSet[index >> 1];
	}
	
	// Again instead of checking if index is even we just update the array at that index equivalent to that check
	// to boost performance
	private void clearBit(int index)
	{
		dataSet[index >> 1] = (index & 1) == 1;
	}
	
	public void runSieve()
	{
		int factor = 3;
		int q = (int) Math.sqrt(sieveSize);
		
		while (factor < q)
		{
			for (int num = factor; num <= sieveSize; num++)
			{
				if (getBit(num))
				{
					factor = num;
					break;
				}
			}
			
			for (int num = factor * factor; num <= sieveSize; num += factor * 2)
				clearBit(num);
			
			factor += 2;
		}
	}
	
	public void printResults(boolean showResults, double duration, int passes)
	{
		if (showResults)
		{
			System.out.print("2, ");
		}
		
		int count = 1;
		for (int num = 3; num <= this.sieveSize; num++)
		{
			if (getBit(num))
			{
				if (showResults)
				{
					System.out.print(num + ", ");
				}
				
				count++;
			}
		}
		
		if (showResults)
		{
			System.out.println();
		}
		
		System.out.printf("Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %s%n", passes, duration, (duration / passes), sieveSize, count, validateResults());

        // Following 2 lines added by rbergen to conform to drag race output format
		System.out.println();
		System.out.printf("MansenC-native;%d;%f;1;algorithm=base,faithful=yes\n", passes, duration);
		
	}
	
	public static void main(String[] args)
	{
		long start = System.currentTimeMillis();
		int passes = 0;
		PrimeSieveJava sieve = null;
		
		while ((System.currentTimeMillis() - start) < 5000)
		{
			sieve = new PrimeSieveJava(1000000);
			sieve.runSieve();
			passes++;
		}
		
		long delta = System.currentTimeMillis() - start;
		if (sieve != null)
		{
			sieve.printResults(false, delta / 1000d, passes);
		}
	}
	
	static
	{
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
