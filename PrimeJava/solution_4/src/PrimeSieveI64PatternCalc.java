import java.util.Arrays;

public class PrimeSieveI64PatternCalc extends PrimeSieveBase{
	private static final int SHIFT_SIZE = 6;
	private static final int SHIFT_SIZE_ADD = SHIFT_SIZE + 1;// one more because we don't store even numbers
	private static final int SIZE = 1 << SHIFT_SIZE;
	private static final int PATTERN_SIZE = 8;
	
	private final long[] dataSet;
	private final long[] pattern, firstPattern;
	
	public PrimeSieveI64PatternCalc(int sieveSize) {
		super(sieveSize);
		type = "other";
		dataSet = new long[((sieveSize + 1) >> 1) / SIZE + 1];
		pattern = new long[PATTERN_SIZE];
		firstPattern = new long[PATTERN_SIZE];
	}

	public boolean getBit(int index) {
		return (dataSet[index >> SHIFT_SIZE_ADD] & (1l << (index >> 1))) == 0;
	}
	
	public void clearBits(int factor) {
		if(factor < PATTERN_SIZE) {
			Arrays.fill(firstPattern, 0);
			Arrays.fill(pattern, 0);
			int vs = factor << SHIFT_SIZE_ADD;
			for (int num = factor * factor; num <= vs; num += factor * 2) {
				firstPattern[num >> SHIFT_SIZE_ADD] |= (1l << (num >> 1));
			}
			for (int num = factor; num <= vs; num += factor * 2) {
				pattern[num >> SHIFT_SIZE_ADD] |= (1l << (num >> 1));
			}
			int idxBase = ((factor * factor) >> SHIFT_SIZE_ADD);
			int first = factor - idxBase;
			for(int i = 0; i < first;i++) {
				int patternIdx = (i + idxBase) % factor;
				dataSet[i + idxBase] |= firstPattern[patternIdx];
			}
			for(int i = first; i < dataSet.length - idxBase;i++) {
				int patternIdx = (i + idxBase) % factor;
				dataSet[i + idxBase] |= pattern[patternIdx];
			}
		} else {
			for (int num = factor * factor; num <= sieveSize; num += factor * 2) {
				dataSet[num >> SHIFT_SIZE_ADD] = dataSet[num >> SHIFT_SIZE_ADD] | (1l << (num >> 1));
			}
		}
	}
	
	public static void main(String[] args) throws InterruptedException {
		PrimeSieveBase.run(() -> new PrimeSieveI64PatternCalc(1000000), args);
	}
}
