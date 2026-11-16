import jdk.incubator.vector.ByteVector;
import jdk.incubator.vector.VectorSpecies;

// Sieve of Eratosthenes over the odd numbers. Bit i is the number 2i+1 and a set bit
// means composite, so a new buffer starts with everything prime.
final class PrimeSieve {
    private static final VectorSpecies<Byte> SPECIES = ByteVector.SPECIES_256;

    // MASK_ROOM[m] is the room for the patterns of the primes from 3 to m, 32 bytes per
    // unit of factor.
    private static final int[] MASK_ROOM = new int[256];

    static {
        int room = 0;
        for (int p = 3; p < MASK_ROOM.length; p++) {
            boolean prime = p % 2 == 1;
            for (int d = 3; d * d <= p && prime; d += 2) {
                prime = p % d != 0;
            }
            if (prime) {
                room += 32 * p;
            }
            MASK_ROOM[p] = room;
        }
    }

    private final int size;
    private final int halfLimit; // how many odd numbers: 1, 3, 5, ... up to size
    private final int vectorMax; // largest factor on the vector path, at most 255
    private final boolean unchecked;
    private final byte[] bits;

    PrimeSieve(int size, int vectorMax, boolean unchecked) {
        if (size < 0 || vectorMax < 0 || vectorMax >= MASK_ROOM.length) {
            throw new IllegalArgumentException();
        }
        this.size = size;
        this.halfLimit = (size + 1) / 2;
        this.vectorMax = vectorMax;
        this.unchecked = unchecked;
        // Rounded up to whole 256-bit vectors.
        this.bits = new byte[((halfLimit + 255) >> 8) << 5];
    }

    // Sieves one segment at a time so the marking stays in L1. In each segment the
    // factors found so far carry on from where they stopped, then the search for new
    // ones goes on. By then every smaller factor has marked the segment.
    void run(int segmentBytes) {
        final byte[] bits = this.bits;
        final int limit = halfLimit;
        if (limit < 2) {
            return; // no odd number above 1
        }

        // Room for every prime up to sqrt(size): pi(x) < 1.26 x / ln x.
        double root = Math.sqrt(size);
        int maxFactors = (int) (1.26 * root / Math.log(root + 2)) + 8;
        int[] factors = new int[maxFactors];
        int[] next = new int[maxFactors];

        // The vector patterns are built once per pass, 32 * factor bytes each.
        byte[] masks = new byte[MASK_ROOM[vectorMax]];
        int[] maskAt = new int[64];
        int vectorCount = 0;
        int maskUsed = 0;

        int found = 0;
        int h = 1; // next candidate to check
        boolean searching = true;
        int segmentBits = segmentBytes * 8;

        for (int segmentStart = 0; segmentStart < limit; segmentStart += segmentBits) {
            int end = Math.min(segmentStart + segmentBits, limit);

            for (int f = 0; f < vectorCount; f++) {
                next[f] = markVector(bits, factors[f], next[f], end, masks, maskAt[f]);
            }
            for (int f = vectorCount; f < found; f++) {
                next[f] = markLarge(bits, factors[f], next[f], end);
            }

            while (searching) {
                while (h < end && (bits[h >> 3] & (1 << (h & 7))) != 0) {
                    h++;
                }
                if (h >= end) {
                    break; // the search continues in the next segment
                }
                int factor = 2 * h + 1;
                int start = 2 * h * (h + 1); // index of factor * factor
                if (start >= limit) {
                    searching = false;
                    break;
                }
                factors[found] = factor;
                if (factor <= vectorMax) {
                    buildMasks(masks, maskUsed, factor);
                    maskAt[vectorCount++] = maskUsed;
                    next[found] = markVector(bits, factor, start, end, masks, maskUsed);
                    maskUsed += 32 * factor;
                } else {
                    next[found] = markLarge(bits, factor, start, end);
                }
                found++;
                h++;
            }
        }
    }

    // Writes the pattern of factor into 32 * factor zeroed bytes: 64 marks, then two
    // copies.
    static void buildMasks(byte[] masks, int at, int factor) {
        for (int j = factor >> 1; j < 64 * factor; j += factor) {
            masks[at + (j >> 3)] |= (byte) (1 << (j & 7));
        }
        System.arraycopy(masks, at, masks, at + 8 * factor, 8 * factor);
        System.arraycopy(masks, at, masks, at + 16 * factor, 16 * factor);
    }

    // Small factors, 256 bits at a time. The pattern repeats after `factor` vectors.
    static int markVector(byte[] bits, int factor, int idx, int limit, byte[] masks, int maskAt) {
        int v0 = (idx + 255) >> 8; // first whole vector at or after idx
        int v1 = limit >> 8;       // end of the whole vectors below limit
        if (v0 >= v1) {
            return markEach(bits, factor, idx, limit);
        }
        idx = markEach(bits, factor, idx, v0 << 8);

        // Runs of vectors up to the end of the pattern, then start over.
        int period = factor * 32;
        int at = v0 % factor * 32;
        for (int offset = v0 * 32, stop = v1 * 32; offset < stop; at = 0) {
            int n = Math.min(period - at, stop - offset);
            int mask = maskAt + at;
            for (int i = 0; i < n; i += 32) {
                ByteVector.fromArray(SPECIES, bits, offset + i)
                        .or(ByteVector.fromArray(SPECIES, masks, mask + i))
                        .intoArray(bits, offset + i);
            }
            offset += n;
        }

        // Skip to the first multiple after the vectors and finish bit by bit.
        int gap = (v1 << 8) - idx;
        idx += (gap + factor - 1) / factor * factor;
        return markEach(bits, factor, idx, limit);
    }

    // Large factors: the "extreme" loop, then the rest one at a time.
    private int markLarge(byte[] bits, int factor, int idx, int limit) {
        idx = unchecked
                ? Unchecked.markEights(bits, factor, idx, limit)
                : markEights(bits, factor, idx, limit);
        return markEach(bits, factor, idx, limit);
    }

    // Eight multiples of p in a row span exactly p bytes, so one round sets one bit in
    // each of eight bytes at fixed offsets. Runs while all eight fall below limit and
    // returns the next index.
    static int markEights(byte[] bits, int p, int idx, int limit) {
        int r = idx & 7;
        int o1 = (r + p) >> 3;
        int o2 = (r + 2 * p) >> 3;
        int o3 = (r + 3 * p) >> 3;
        int o4 = (r + 4 * p) >> 3;
        int o5 = (r + 5 * p) >> 3;
        int o6 = (r + 6 * p) >> 3;
        int o7 = (r + 7 * p) >> 3;
        byte m0 = (byte) (1 << r);
        byte m1 = (byte) (1 << ((r + p) & 7));
        byte m2 = (byte) (1 << ((r + 2 * p) & 7));
        byte m3 = (byte) (1 << ((r + 3 * p) & 7));
        byte m4 = (byte) (1 << ((r + 4 * p) & 7));
        byte m5 = (byte) (1 << ((r + 5 * p) & 7));
        byte m6 = (byte) (1 << ((r + 6 * p) & 7));
        byte m7 = (byte) (1 << ((r + 7 * p) & 7));
        int i = idx >> 3;
        int end = (limit >> 3) - o7;
        for (; i < end; i += p) {
            bits[i] |= m0;
            bits[i + o1] |= m1;
            bits[i + o2] |= m2;
            bits[i + o3] |= m3;
            bits[i + o4] |= m4;
            bits[i + o5] |= m5;
            bits[i + o6] |= m6;
            bits[i + o7] |= m7;
        }
        return i << 3 | r;
    }

    static int markEach(byte[] bits, int factor, int idx, int limit) {
        for (; idx < limit; idx += factor) {
            bits[idx >> 3] |= (byte) (1 << (idx & 7));
        }
        return idx;
    }

    boolean isPrime(int n) {
        if (n < 2 || n > size) {
            return false;
        }
        if ((n & 1) == 0) {
            return n == 2;
        }
        int i = n >> 1;
        return (bits[i >> 3] & (1 << (i & 7))) == 0;
    }

    int countPrimes() {
        if (size < 2) {
            return 0;
        }
        int composites = 0;
        int full = halfLimit >> 3;
        for (int i = 0; i < full; i++) {
            composites += Integer.bitCount(bits[i] & 0xFF);
        }
        int rest = halfLimit & 7;
        if (rest != 0) {
            composites += Integer.bitCount(bits[full] & ((1 << rest) - 1));
        }
        // 1 is counted but is not prime, 2 is prime but not counted. They cancel out.
        return halfLimit - composites;
    }
}
