public class PrimeSieveBool extends PrimeSieveBase{

	private boolean[] dataSet;
	
	public PrimeSieveBool(int sieveSize) {
		super(sieveSize);
		bits = 8;
		dataSet = new boolean[(sieveSize + 1) >> 1];
	}

	public boolean getBit(int index) {
		return !dataSet[index >> 1];
	}

	public void clearBit(int index) {
		dataSet[index >> 1] = true;
	}
	
	public static void main(String[] args) throws InterruptedException {
		PrimeSieveBase.run(() -> new PrimeSieveBool(1000000), args);
	}
}
