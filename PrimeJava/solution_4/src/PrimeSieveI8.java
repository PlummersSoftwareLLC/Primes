public class PrimeSieveI8 extends PrimeSieveBaseBit{
	private static final int SHIFT_SIZE = 3;
	private static final int SHIFT_SIZE_ADD = SHIFT_SIZE + 1;
	private static final int SIZE = 1 << SHIFT_SIZE;
	private static final int MOD = SIZE - 1;
	
	private final byte[] dataSet;
	
	public PrimeSieveI8(int sieveSize) {
		super(sieveSize);
		dataSet = new byte[((sieveSize + 1) >> 1) / SIZE + 1];
	}

	public boolean getBit(int index) {
		return (dataSet[index >> SHIFT_SIZE_ADD] & (1 << ((index >> 1) & MOD))) == 0;
	}

	public void clearBit(int index) {
		dataSet[index >> SHIFT_SIZE_ADD] |= (1 << ((index >> 1) & MOD));
	}
	
	public static void main(String[] args) throws InterruptedException {
		PrimeSieveBase.run(() -> new PrimeSieveI8(1000000), args);
	}
}
