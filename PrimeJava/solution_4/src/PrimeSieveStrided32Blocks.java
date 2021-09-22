public class PrimeSieveStrided32Blocks extends PrimeSieveBase{
	private static final int SHIFT_SIZE = 5;
	private static final int SHIFT_SIZE_ADD = SHIFT_SIZE + 1;// one more because we don't store even numbers
	private static final int SIZE = 1 << SHIFT_SIZE;
	private static final int BLOCK_SIZE_BITS = 1024 * 128;
	private static final int BLOCK_SIZE = BLOCK_SIZE_BITS / SIZE;
	
	
	private final int[][] dataBlocks;
	private final int blocks;
	
	public PrimeSieveStrided32Blocks(int sieveSize) {
		super(sieveSize);
		int size = ((sieveSize + 1) >> 1) / SIZE + 1;
		blocks = size / BLOCK_SIZE + 1;
		dataBlocks = new int[blocks][BLOCK_SIZE];
	}

	public boolean getBit(int index) {
		int idx = index >> SHIFT_SIZE_ADD;
		int block = idx / BLOCK_SIZE;
		idx = idx - block * BLOCK_SIZE;// index into selected block
		return (dataBlocks[block][idx] & (1 << (index >> 1))) == 0;
	}

	public void clearBits(int factor) {
		int ff = factor * factor;
		int factor2 = factor << 1;
		int factor4 = factor2 << 1;
		int[] strideStarts = new int[SIZE];
		for(int stride = 0; stride < SIZE;stride++) {
			strideStarts[stride] = (ff + stride * factor2) >> SHIFT_SIZE_ADD;
		}
		for(int block = 0; block < blocks;block++) {
			int[] dataSet = dataBlocks[block];
			for(int stride = 0; stride < SIZE;stride++) {
				int mask = 1 << ((ff + stride * factor2) >> 1);
				int start0 = strideStarts[stride] - block * BLOCK_SIZE;
				int start1 = start0 + factor;
				int start2 = start1 + factor;
				int start3 = start2 + factor;
				
				while(start3 < dataSet.length) {
					dataSet[start0] |= mask;
					start0 += factor4;
					dataSet[start1] |= mask;
					start1 += factor4;
					dataSet[start2] |= mask;
					start2 += factor4;
					dataSet[start3] |= mask;
					start3 += factor4;
				}
				while(start1 < dataSet.length) {
					dataSet[start0] |= mask;
					start0 += factor2;
					dataSet[start1] |= mask;
					start1 += factor2;
				}
				while(start0 < dataSet.length) {
					dataSet[start0] |= mask;
					start0 += factor;
				}
				// remember where we were with this stride for the next block
				strideStarts[stride] = start0 + block * BLOCK_SIZE;
			}
		}
	}
	
	public String toString() {
		return super.toString()+(BLOCK_SIZE_BITS / 8192)+"k";
	}
	
	public static void main(String[] args) throws InterruptedException {
		PrimeSieveBase.run(() -> new PrimeSieveStrided32Blocks(1000000), args);
	}
}
