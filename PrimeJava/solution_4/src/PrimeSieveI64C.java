public class PrimeSieveI64C extends PrimeSieveBaseBit{
	private static final int SHIFT_SIZE = 6;
	private static final int SHIFT_SIZE_ADD = SHIFT_SIZE + 1;// one more because we don't store even numbers
	private static final int SIZE = 1 << SHIFT_SIZE;
	private static final int MOD = (SIZE << 1) - 1;
	
	// bitset of odd numbers, zero means its a prime
	private final long[] dataSet;
	
	private static final long[] masks;
	
	static {
		// double size so that we don't need to halve index in clearBit
		masks = new long[SIZE << 1];
		for(int index = 0; index < masks.length;index++) {
			// index >> 1 because of not storing even numbers
			masks[index] = 1l << (index >> 1);
		}
	}
	
	public PrimeSieveI64C(int sieveSize) {
		super(sieveSize);
		dataSet = new long[((sieveSize + 1) >> 1) / SIZE + 1];
	}

	public boolean getBit(int index) {		
		return (dataSet[index >> SHIFT_SIZE_ADD] & masks[index & MOD]) == 0;
	}

	public void clearBit(int index) {
		dataSet[index >> SHIFT_SIZE_ADD] = dataSet[index >> SHIFT_SIZE_ADD] | (masks[index & MOD]);
	}
	
	public static void main(String[] args) throws InterruptedException {
		PrimeSieveBase.run(() -> new PrimeSieveI64C(1000000), args);
	}
}
