public class PrimeSieveI32CUnroll extends PrimeSieveBase{
	private static final int SHIFT_SIZE = 5;
	private static final int SHIFT_SIZE_ADD = SHIFT_SIZE + 1;// one more because we don't store even numbers
	private static final int SIZE = 1 << SHIFT_SIZE;
	private static final int MOD = (SIZE << 1) - 1;
	// bitset of odd numbers, zero means its a prime
	private final int[] dataSet;
	
	private static final int[] masks;
	
	static {
		// double size so that we don't need to halve index in clearBit
		masks = new int[SIZE << 1];
		for(int index = 0; index < masks.length;index++) {
			// index >> 1 because of not storing even numbers
			masks[index] = 1 << (index >> 1);
		}
	}
	
	public PrimeSieveI32CUnroll(int sieveSize) {
		super(sieveSize);
		dataSet = new int[((sieveSize + 1) >> 1) / SIZE + 1];
	}

	public boolean getBit(int index) {		
		return (dataSet[index >> SHIFT_SIZE_ADD] & (1 << (index >> 1))) == 0;
	}

	public void clearBits(int factor) {
		int i0 = factor * factor;
		int factor2 = factor << 1;
		int factor4 = factor << 2;
		int factor8 = factor << 3;
		int i1 = i0 + factor2;
		int i2 = i1 + factor2;
		int i3 = i2 + factor2;


		while(i3 < sieveSize) {
			dataSet[i0 >> SHIFT_SIZE_ADD] |= (masks[i0 & MOD]);
			i0 += factor8;
			dataSet[i1 >> SHIFT_SIZE_ADD] |= (masks[i1 & MOD]);
			i1 += factor8;
			dataSet[i2 >> SHIFT_SIZE_ADD] |= (masks[i2 & MOD]);
			i2 += factor8;
			dataSet[i3 >> SHIFT_SIZE_ADD] |= (masks[i3 & MOD]);
			i3 += factor8;
		}
		while(i1 < sieveSize) {
			dataSet[i0 >> SHIFT_SIZE_ADD] |= (masks[i0 & MOD]);
			i0 += factor4;
			dataSet[i1 >> SHIFT_SIZE_ADD] |= (masks[i1 & MOD]);
			i1 += factor4;
		}
		while(i0 < sieveSize) {
			dataSet[i0 >> SHIFT_SIZE_ADD] |= (masks[i0 & MOD]);
			i0 += factor2;
		}

	}
	
	public static void main(String[] args) throws InterruptedException {
		PrimeSieveBase.run(() -> new PrimeSieveI32CUnroll(1000000), args);
	}
}
