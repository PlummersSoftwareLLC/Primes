
public abstract class PrimeSieveBaseBit extends PrimeSieveBase{
	public PrimeSieveBaseBit(int size) {
		super(size);
	}

	public abstract void clearBit(int factor);
	
	public void clearBits(int factor) {
		for (int num = factor * factor; num <= sieveSize; num += factor * 2) {
			clearBit(num);
		}
	}
}
