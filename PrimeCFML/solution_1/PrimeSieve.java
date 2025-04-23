public class PrimeSieve {
    private final int limit;
    private final int halfLimit;
    private final byte[] sieve;

    public PrimeSieve(int limit) {
        this.limit = limit;
        this.halfLimit = (limit + 1) / 2;
        this.sieve = new byte[(halfLimit + 7) / 8]; // 8 bits per byte

        // Set all bits to 1 (potential primes)
        for (int i = 0; i < sieve.length; i++) {
            sieve[i] = (byte) 0xFF;
        }
    }

    private boolean isPrimeBit(int index) {
        int byteIndex = index / 8;
        int bitIndex = index % 8;
        return (sieve[byteIndex] & (1 << bitIndex)) != 0;
    }

    private void clearBit(int index) {
        int byteIndex = index / 8;
        int bitIndex = index % 8;
        sieve[byteIndex] &= ~(1 << bitIndex);
    }

    public void runSieve() {
        int sqrtLimit = (int) Math.sqrt(limit);

        for (int p = 3; p <= sqrtLimit; p += 2) {
            int idx = p / 2;
            if (isPrimeBit(idx)) {
                for (int i = (p * p) / 2; i < halfLimit; i += p) {
                    clearBit(i);
                }
            }
        }
    }

    public int countPrimes() {
        int count = 0;
        for (int i = 1; i < halfLimit; i++) {
            if (isPrimeBit(i)) count++;
        }
        return count + 1; // +1 for prime number 2
    }

    public int getLimit() {
        return limit;
    }
}
