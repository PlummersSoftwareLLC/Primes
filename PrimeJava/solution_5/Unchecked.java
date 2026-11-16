import java.lang.reflect.Field;
import sun.misc.Unsafe;

// The "extreme" loop of PrimeSieve without a bounds check on every byte. It goes
// through sun.misc.Unsafe behind one range check per call. The JDK has deprecated that
// access for removal, so this class is only loaded for the "unsafe" variant.
final class Unchecked {
    private static final Unsafe UNSAFE;
    private static final long BASE;

    static {
        try {
            Field field = Unsafe.class.getDeclaredField("theUnsafe");
            field.setAccessible(true);
            UNSAFE = (Unsafe) field.get(null);
            BASE = UNSAFE.arrayBaseOffset(byte[].class);
        } catch (ReflectiveOperationException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private Unchecked() {
    }

    // Same as PrimeSieve.markEights.
    static int markEights(byte[] bits, int p, int idx, int limit) {
        int r = idx & 7;
        long o1 = (r + p) >> 3;
        long o2 = (r + 2 * p) >> 3;
        long o3 = (r + 3 * p) >> 3;
        long o4 = (r + 4 * p) >> 3;
        long o5 = (r + 5 * p) >> 3;
        long o6 = (r + 6 * p) >> 3;
        long o7 = (r + 7 * p) >> 3;
        int m0 = 1 << r;
        int m1 = 1 << ((r + p) & 7);
        int m2 = 1 << ((r + 2 * p) & 7);
        int m3 = 1 << ((r + 3 * p) & 7);
        int m4 = 1 << ((r + 4 * p) & 7);
        int m5 = 1 << ((r + 5 * p) & 7);
        int m6 = 1 << ((r + 6 * p) & 7);
        int m7 = 1 << ((r + 7 * p) & 7);
        int i = idx >> 3;
        int end = (limit >> 3) - (int) o7;
        // The furthest byte touched is i + o7, and the loop keeps that below limit >> 3.
        if (idx < 0 || p < 1 || (limit >> 3) > bits.length) {
            throw new IndexOutOfBoundsException();
        }
        for (; i < end; i += p) {
            long at = BASE + i;
            or(bits, at, m0);
            or(bits, at + o1, m1);
            or(bits, at + o2, m2);
            or(bits, at + o3, m3);
            or(bits, at + o4, m4);
            or(bits, at + o5, m5);
            or(bits, at + o6, m6);
            or(bits, at + o7, m7);
        }
        return i << 3 | r;
    }

    private static void or(byte[] bits, long at, int mask) {
        UNSAFE.putByte(bits, at, (byte) (UNSAFE.getByte(bits, at) | mask));
    }
}
