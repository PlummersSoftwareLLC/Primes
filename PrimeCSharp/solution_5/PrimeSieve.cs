using System;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics;

namespace PrimeSieveCS;

// Sieve of Eratosthenes over the odd numbers. Bit i is the number 2i+1 and a set bit
// means composite, so a new buffer starts with everything prime.
public sealed class PrimeSieve
{
    static PrimeSieve()
    {
        // The same buffer is read as bytes, 64-bit words and vectors.
        if (!BitConverter.IsLittleEndian)
            throw new PlatformNotSupportedException("The sieve assumes a little-endian machine.");
    }

    // Factors up to this are marked with 256-bit vectors. 79 measured the same.
    public const int DefaultVectorDenseMax = 95;

    // MaskRoom[m] is the room for the masks of the primes from 3 to m, four words per
    // unit of factor.
    private static readonly int[] MaskRoom = BuildMaskRoom();

    static int[] BuildMaskRoom()
    {
        var room = new int[256];
        for (int p = 3, sum = 0; p < room.Length; p++)
        {
            bool prime = p % 2 == 1;
            for (int d = 3; d * d <= p && prime; d += 2)
                prime = p % d != 0;
            if (prime)
                sum += 4 * p;
            room[p] = sum;
        }
        return room;
    }

    private readonly int _sieveSize;
    private readonly int _halfLimit; // how many odd numbers: 1, 3, 5, ... up to sieveSize
    private readonly int _vectorMax; // 0 = no vectors
    private readonly byte[] _bits;

    public PrimeSieve(int size, int vectorDenseMax = DefaultVectorDenseMax)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(size);
        ArgumentOutOfRangeException.ThrowIfNegative(vectorDenseMax);
        _sieveSize = size;
        _vectorMax = Vector128.IsHardwareAccelerated ? vectorDenseMax : 0;
        _halfLimit = (int)(((long)size + 1) / 2);
        // Rounded up to whole 256-bit vectors.
        _bits = new byte[(int)(((long)_halfLimit + 255) >> 8 << 5)];
    }

    public int SieveSize => _sieveSize;

    public PrimeSieve RunSieve()
    {
        Span<byte> bytes = _bits;
        var words = MemoryMarshal.Cast<byte, ulong>(bytes);
        int limit = _halfLimit;
        if (limit < 2)
            return this; // no odd candidate above 1

        for (int h = 1; ; h++)
        {
            // Next factor: the first bit that is still clear.
            while ((bytes[h >> 3] & (1 << (h & 7))) != 0)
                h++;

            int factor = 2 * h + 1;
            int start = 2 * h * (h + 1); // index of factor * factor
            if (start >= limit)
                break;

            Mark(bytes, words, factor, start, limit, _vectorMax);
        }
        return this;
    }

    // The same sieve, one segment at a time so the marking stays in L1. In each segment
    // the factors found so far carry on from where they stopped, then the search goes on.
    // A candidate is only tested after every smaller factor has marked its segment.
    public PrimeSieve RunSieveSegmented(int segmentBytes)
    {
        ArgumentOutOfRangeException.ThrowIfLessThan(segmentBytes, 1);
        Span<byte> bytes = _bits;
        var words = MemoryMarshal.Cast<byte, ulong>(bytes);
        int limit = _halfLimit;
        if (limit < 2)
            return this;

        // Factors are odd and at most sqrt(size).
        int maxFactors = (int)(Math.Sqrt(_sieveSize) / 2) + 2;
        var factors = maxFactors <= 1024 ? stackalloc int[maxFactors] : new int[maxFactors];
        var next = maxFactors <= 1024 ? stackalloc int[maxFactors] : new int[maxFactors];
        int found = 0;
        int h = 1; // next candidate to check
        bool searching = true;
        int segmentBits = (int)Math.Min((long)segmentBytes * 8, int.MaxValue);

        // The vector masks are built once per pass, when the factor is found, and kept
        // here for the later segments.
        int half = (_vectorMax + 1) / 2;
        int maskRoom = _vectorMax < MaskRoom.Length ? MaskRoom[_vectorMax] : 4 * (half * half - 1);
        Span<ulong> maskBuffer = maskRoom <= 4096 ? stackalloc ulong[maskRoom + 7] : new ulong[maskRoom + 7];
        // Start on a cache line, so no vector of a mask is split over two.
        nint address = Unsafe.ByteOffset(ref Unsafe.NullRef<ulong>(), ref MemoryMarshal.GetReference(maskBuffer));
        Span<ulong> maskStore = maskBuffer.Slice((int)(-address >> 3) & 7);
        Span<int> maskAt = maxFactors <= 1024 ? stackalloc int[maxFactors] : new int[maxFactors];
        int maskUsed = 0;

        for (long segmentStart = 0; segmentStart < limit; segmentStart += segmentBits)
        {
            int end = (int)Math.Min(segmentStart + segmentBits, limit);

            for (int f = 0; f < found; f++)
            {
                int known = factors[f];
                next[f] = known <= _vectorMax
                    ? MarkDenseVector(bytes, words, known, next[f], end, maskStore.Slice(maskAt[f], 4 * known))
                    : MarkScalar(bytes, words, known, next[f], end);
            }

            while (searching)
            {
                while (h < end && (bytes[h >> 3] & (1 << (h & 7))) != 0)
                    h++;
                if (h >= end)
                    break; // the search continues in the next segment

                int factor = 2 * h + 1;
                int start = 2 * h * (h + 1);
                if (start >= limit)
                {
                    searching = false;
                    break;
                }
                factors[found] = factor;
                if (factor <= _vectorMax)
                {
                    Span<ulong> masks = maskStore.Slice(maskUsed, 4 * factor);
                    BuildMasks(masks, factor);
                    maskAt[found] = maskUsed;
                    maskUsed += 4 * factor;
                    next[found] = MarkDenseVector(bytes, words, factor, start, end, masks);
                }
                else
                {
                    next[found] = MarkScalar(bytes, words, factor, start, end);
                }
                found++;
                h++;
            }
        }
        return this;
    }

    static int Mark(Span<byte> bytes, Span<ulong> words, int factor, int idx, int limit, int vectorMax)
    {
        if (factor <= vectorMax)
            return MarkDenseVector(bytes, words, factor, idx, limit);
        return MarkScalar(bytes, words, factor, idx, limit);
    }

    static int MarkScalar(Span<byte> bytes, Span<ulong> words, int factor, int idx, int limit)
    {
        if (factor <= Markers.DenseMax)
            return MarkDense(bytes, words, factor, idx, limit);
        return MarkSparse(bytes, factor, idx, limit);
    }

    // The masks of one factor: `factor` vectors, 4 * factor words. The pattern repeats
    // every `factor` words, so mark the first quarter bit by bit and copy it. Marking all
    // four quarters was three times slower.
    internal static void BuildMasks(Span<ulong> maskWords, int factor)
    {
        maskWords.Slice(0, factor).Clear();
        for (int j = factor >> 1; j < 64 * factor; j += factor)
            maskWords[j >> 6] |= 1UL << (j & 63);
        maskWords.Slice(0, factor).CopyTo(maskWords.Slice(factor));
        maskWords.Slice(0, 2 * factor).CopyTo(maskWords.Slice(2 * factor));
    }

    internal static int MarkDenseVector(Span<byte> bytes, Span<ulong> words, int factor, int idx, int limit)
    {
        Span<ulong> maskWords = stackalloc ulong[4 * factor];
        BuildMasks(maskWords, factor);
        return MarkDenseVector(bytes, words, factor, idx, limit, maskWords);
    }

    // 256-bit vectors where the CPU has them (x64 with AVX2), 128-bit otherwise (arm64).
    static int MarkDenseVector(Span<byte> bytes, Span<ulong> words, int factor, int idx, int limit, Span<ulong> maskWords)
    {
        return Vector256.IsHardwareAccelerated
            ? MarkDenseVector256(bytes, words, factor, idx, limit, maskWords)
            : MarkDenseVector128(bytes, words, factor, idx, limit, maskWords);
    }

    // The same as the 256-bit loop below, with vectors half as wide. The masks are the
    // same words, so the pattern simply repeats after `factor` of these vectors too.
    internal static int MarkDenseVector128(Span<byte> bytes, Span<ulong> words, int factor, int idx, int limit, Span<ulong> maskWords)
    {
        int v0 = (idx + 127) >> 7;
        int v1 = limit >> 7;
        if (v0 >= v1)
            return MarkEach(bytes, factor, idx, limit);
        idx = MarkEach(bytes, factor, idx, v0 << 7);

        var masks = MemoryMarshal.Cast<ulong, Vector128<ulong>>(maskWords.Slice(0, 2 * factor));
        var vectors = MemoryMarshal.Cast<ulong, Vector128<ulong>>(words);

        int v = v0;
        int r = v0 % factor;
        if (r != 0)
        {
            int stop = Math.Min(v1, v + factor - r);
            for (; v < stop; v++, r++)
                vectors[v] |= masks[r];
        }
        for (; v + factor <= v1; v += factor)
        {
            Span<Vector128<ulong>> run = vectors.Slice(v, masks.Length);
            for (int i = 0; i < masks.Length; i++)
                run[i] |= masks[i];
        }
        for (r = 0; v < v1; v++, r++)
            vectors[v] |= masks[r];

        int gap = (v1 << 7) - idx;
        idx += (gap + factor - 1) / factor * factor;
        return MarkEach(bytes, factor, idx, limit);
    }

    // Small factors, 256 bits per store. The multiples repeat every `factor` vectors,
    // so vector v gets mask number v % factor.
    internal static int MarkDenseVector256(Span<byte> bytes, Span<ulong> words, int factor, int idx, int limit, Span<ulong> maskWords)
    {
        int v0 = (idx + 255) >> 8; // first whole vector at or after idx
        int v1 = limit >> 8;       // end of the whole vectors below limit
        if (v0 >= v1)
            return MarkEach(bytes, factor, idx, limit);
        idx = MarkEach(bytes, factor, idx, v0 << 8);

        var masks = MemoryMarshal.Cast<ulong, Vector256<ulong>>(maskWords);
        var vectors = MemoryMarshal.Cast<ulong, Vector256<ulong>>(words);

        int v = v0;
        int r = v0 % factor;
        if (r != 0)
        {
            int stop = Math.Min(v1, v + factor - r);
            for (; v < stop; v++, r++)
                vectors[v] |= masks[r];
        }
        for (; v + factor <= v1; v += factor)
        {
            Span<Vector256<ulong>> run = vectors.Slice(v, masks.Length);
            for (int i = 0; i < masks.Length; i++)
                run[i] |= masks[i];
        }
        for (r = 0; v < v1; v++, r++)
            vectors[v] |= masks[r];

        // Skip to the first multiple after the vectors and finish bit by bit.
        int gap = (v1 << 8) - idx;
        idx += (gap + factor - 1) / factor * factor;
        return MarkEach(bytes, factor, idx, limit);
    }

    // Medium factors: whole blocks of `factor` words, 64 multiples each. Like all the
    // Mark methods it returns the index of the next multiple.
    static int MarkDense(Span<byte> bytes, Span<ulong> words, int factor, int idx, int limit)
    {
        int blockBits = factor << 6;
        int k0 = (idx + blockBits - 1) / blockBits;
        int k1 = limit / blockBits;
        if (k0 < k1)
        {
            idx = MarkEach(bytes, factor, idx, k0 * blockBits);
            Markers.DenseBlocks(factor, words, k0, k1);
            idx = k1 * blockBits + (factor >> 1); // first multiple of the next block
        }
        return MarkEach(bytes, factor, idx, limit);
    }

    static int MarkSparse(Span<byte> bytes, int factor, int idx, int limit)
    {
        idx = Markers.Sparse(factor, bytes, idx, limit);
        return MarkEach(bytes, factor, idx, limit);
    }

    static int MarkEach(Span<byte> bytes, int factor, int idx, int limit)
    {
        for (; idx < limit; idx += factor)
            bytes[idx >> 3] |= (byte)(1 << (idx & 7));
        return idx;
    }

    public bool IsPrime(int n)
    {
        if (n < 2 || n > _sieveSize)
            return false;
        if ((n & 1) == 0)
            return n == 2;
        int i = n >> 1;
        return (_bits[i >> 3] & (1 << (i & 7))) == 0;
    }

    public int CountPrimes()
    {
        if (_sieveSize < 2)
            return 0;
        
        ReadOnlySpan<ulong> words = MemoryMarshal.Cast<byte, ulong>(_bits);
        int composites = 0;
        int full = _halfLimit >> 6;
        
        for (int w = 0; w < full; w++)
            composites += BitOperations.PopCount(words[w]);
        
        int rest = _halfLimit & 63;
        if (rest != 0)
            composites += BitOperations.PopCount(words[full] & ((1UL << rest) - 1));
        
        // 1 is counted but is not prime, 2 is prime but not counted. They cancel out.
        return _halfLimit - composites;
    }
}
