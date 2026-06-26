module app;

import core.bitop : rol;
import core.stdc.stdlib : malloc, free, calloc;
import core.time : MonoTime, Duration, dur;
import std.algorithm : filter;
import std.algorithm.sorting : sort;
import std.array : array;
import std.conv : to;
import std.format : format;
import std.getopt;
import std.math : sqrt;
import std.stdio : stderr, stdout, write, writeln, writefln;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------
enum U8_BITS = 8;
enum U32_BITS = 32;
enum U64_BITS = 64;
enum BLOCK_SIZE_DEFAULT = 16 * 1024;
enum BLOCK_SIZE_SMALL = 4 * 1024;

// ---------------------------------------------------------------------------
// Utility functions
// ---------------------------------------------------------------------------
pure nothrow @nogc @safe size_t squareStart(size_t skip) { return skip * skip / 2; }

pure nothrow @nogc @safe size_t minimumStart(size_t skip) { return skip / 2 + skip; }

// ---------------------------------------------------------------------------
// PrimeValidator
// ---------------------------------------------------------------------------
struct PrimeValidator
{
    static immutable size_t[8] limits = [10, 100, 1000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000];
    static immutable size_t[8] counts = [4, 25, 168, 1229, 9592, 78498, 664579, 5761455];

    bool isValid(size_t sieveSize, size_t result) const pure nothrow @safe
    {
        foreach (i, ref lim; limits)
        {
            if (lim == sieveSize)
                return result == counts[i];
        }
        return true;
    }
}

// ---------------------------------------------------------------------------
// Compile-time mask/index helpers for UnrolledHybrid
// ---------------------------------------------------------------------------
pure nothrow @nogc @safe size_t patternEquivalentSkip(size_t skip, size_t bits)
{
    auto idx = (skip - 3) / 2;
    auto modulo = idx % bits;
    return (modulo * 2) + 3;
}

// CTFE functions for mask/index computation
pure nothrow @nogc size_t[64] computeModuloPattern64(int SKIP)()
{
    size_t[64] p;
    auto start = SKIP / 2;
    foreach (i; 0 .. 64)
        p[i] = (start + i * SKIP) % 64;
    return p;
}

pure nothrow @nogc size_t[64] computeIndexPattern64(int SKIP)()
{
    size_t[64] p;
    auto start = SKIP / 2;
    foreach (i; 0 .. 64)
        p[i] = (start + i * SKIP) / 64;
    return p;
}

pure nothrow @nogc ulong[64] computeMaskPattern64(int SKIP)()
{
    ulong[64] masks;
    auto mp = computeModuloPattern64!SKIP();
    foreach (i; 0 .. 64)
        masks[i] = 1UL << mp[i];
    return masks;
}

pure nothrow @nogc ubyte[8] computeMaskPatternU8(int SKIP)()
{
    ubyte[8] masks;
    size_t[8] p;
    auto start = SKIP / 2;
    foreach (i; 0 .. 8)
        p[i] = (start + i * SKIP) % 8;
    foreach (i; 0 .. 8)
        masks[i] = cast(ubyte)(1 << p[i]);
    return masks;
}

pure nothrow @nogc size_t[8] computeIndexPatternU8(int SKIP)()
{
    size_t[8] p;
    auto start = SKIP / 2;
    foreach (i; 0 .. 8)
        p[i] = (start + i * SKIP) / 8;
    return p;
}

// Runtime version for sparse reset (uses actual skip, not template param)
pure nothrow @nogc @safe size_t[8] computeIndexPatternU8Runtime(size_t skip)
{
    size_t[8] p;
    auto start = skip / 2;
    foreach (i; 0 .. 8)
        p[i] = (start + i * skip) / 8;
    return p;
}

// ---------------------------------------------------------------------------
// Mixin helpers for dense dispatch
// ---------------------------------------------------------------------------
string genDenseDispatch(string func, string arg)
{
    string code;
    code ~= "switch (skip) {\n";
    for (int i = 3; i <= 129; i += 2)
        code ~= format!"case %d: %s!%d(%s); return;\n"(i, func, i, arg);
    code ~= "default: break;\n}";
    return code;
}

string genSparseDispatch(string func, string args)
{
    string code;
    code ~= "switch (equivSkip) {\n";
    for (int i = 3; i <= 17; i += 2)
        code ~= format!"case %d: %s!%d(%s); return;\n"(i, func, i, args);
    code ~= "default: break;\n}";
    return code;
}

// ===== ALLOCATOR HELPERS =====
// Allocate a zero-initialized ulong array
ulong[] allocWords(size_t n) @nogc nothrow @trusted
{
    auto p = cast(ulong*)calloc(n, ulong.sizeof);
    return p ? p[0 .. n] : null;
}

// Allocate a 0xFF-initialized ubyte array
ubyte[] allocBytesSet(size_t n) @nogc nothrow @trusted
{
    auto p = cast(ubyte*)malloc(n);
    if (!p) return null;
    auto slice = p[0 .. n];
    slice[] = 0xFF;
    return slice;
}

// Allocate a 0x00-initialized ubyte array
ubyte[] allocBytesZero(size_t n) @nogc nothrow @trusted
{
    auto p = cast(ubyte*)calloc(n, 1);
    return p ? p[0 .. n] : null;
}

// Allocate a ~0UL-initialized ulong array
ulong[] allocWordsOnes(size_t n) @nogc nothrow @trusted
{
    auto p = cast(ulong*)malloc(n * ulong.sizeof);
    if (!p) return null;
    auto slice = p[0 .. n];
    slice[] = ~0UL;
    return slice;
}

void freeSlice(T)(T[] slice) @nogc nothrow @trusted
{
    if (slice.ptr) free(slice.ptr);
}

// ---------------------------------------------------------------------------
// Dense reset functions — each specialized at compile time for a given SKIP
// ---------------------------------------------------------------------------
pragma(inline, false)
void resetDense(int SKIP)(ulong[] words) @nogc nothrow @trusted
{
    enum indices = computeIndexPattern64!SKIP();
    enum masks = computeMaskPattern64!SKIP();

    auto start = squareStart(SKIP);
    auto startChunk = start / 64 / SKIP * SKIP;

    size_t ci = startChunk;
    while (ci + SKIP <= words.length)
    {
        static foreach (i; 0 .. 64)
            words[ci + indices[i]] |= masks[i];
        ci += SKIP;
    }

    // remainder
    if (ci < words.length)
    {
        foreach (i; 0 .. 64)
        {
            auto idx = ci + indices[i];
            if (idx < words.length)
                words[idx] |= masks[i];
            else
                break;
        }
    }

    auto factorIdx = SKIP / 2;
    auto factorWord = factorIdx / 64;
    auto factorBit = factorIdx % 64;
    if (factorWord < words.length)
        words[factorWord] &= ~(1UL << factorBit);
}

// ---------------------------------------------------------------------------
// Sparse reset function
// ---------------------------------------------------------------------------
pragma(inline, false)
void resetSparse(int EQUIV_SKIP)(ulong[] words, size_t skip) @nogc nothrow @trusted
{
    enum masks = computeMaskPatternU8!(EQUIV_SKIP);
    auto indices = computeIndexPatternU8Runtime(skip);

    auto bytes = cast(ubyte[])words;

    auto start = squareStart(skip);
    auto startChunk = start / 8 / skip * skip;

    size_t ci = startChunk;
    while (ci + skip <= bytes.length)
    {
        static foreach (i; 0 .. 8)
            bytes[ci + indices[i]] |= masks[i];
        ci += skip;
    }

    if (ci < bytes.length)
    {
        foreach (i; 0 .. 8)
        {
            if (ci + indices[i] < bytes.length)
                bytes[ci + indices[i]] |= masks[i];
        }
    }
}

// ---------------------------------------------------------------------------
// ByteStorage — simple ubyte vector
// ---------------------------------------------------------------------------
struct ByteStorage
{
    ubyte[] data;

    static ByteStorage createTrue(size_t size) @nogc nothrow @trusted
    {
        ByteStorage s;
        auto p = cast(ubyte*)malloc(size);
        if (!p) return s;
        s.data = p[0 .. size];
        s.data[] = 1;
        return s;
    }

    pragma(inline, true)
    void resetFlags(size_t skip) @nogc nothrow @trusted
    {
        size_t i = squareStart(skip);

        auto endUnrolled = (i + skip * 3 <= data.length) ? data.length - skip * 3 : 0;
        while (i < endUnrolled)
        {
            data[i] = 0;
            data[i + skip] = 0;
            data[i + skip * 2] = 0;
            data[i + skip * 3] = 0;
            i += skip * 4;
        }

        while (i < data.length)
        {
            data[i] = 0;
            i += skip;
        }
    }

    pragma(inline, true)
    bool get(size_t index) const @nogc nothrow @safe
    {
        if (index >= data.length) return false;
        return data[index] == 1;
    }
}

// ---------------------------------------------------------------------------
// BitStorage — u64 word-based bit vector
// ---------------------------------------------------------------------------
struct BitStorage
{
    ulong[] words;
    size_t lengthBits;

    static BitStorage createTrue(size_t size) @nogc nothrow @trusted
    {
        BitStorage s;
        s.lengthBits = size;
        auto nw = size / U64_BITS + (size % U64_BITS != 0 ? 1 : 0);
        s.words = allocWordsOnes(nw);
        return s;
    }

    pragma(inline, true)
    void resetFlags(size_t skip) @nogc nothrow @trusted
    {
        auto i = squareStart(skip);
        auto totalBits = words.length * U64_BITS;

        while (i < totalBits)
        {
            auto wordIdx = i / U64_BITS;
            auto bitIdx = i % U64_BITS;
            words[wordIdx] &= ~(1UL << bitIdx);
            i += skip;
        }
    }

    pragma(inline, true)
    bool get(size_t index) const @nogc nothrow @safe
    {
        if (index >= lengthBits) return false;
        return (words[index / U64_BITS] & (1UL << (index % U64_BITS))) != 0;
    }
}

// ---------------------------------------------------------------------------
// BitStorageRotate — u64 word-based bit vector using rotate for mask
// ---------------------------------------------------------------------------
struct BitStorageRotate
{
    ulong[] words;
    size_t lengthBits;

    static BitStorageRotate createTrue(size_t size) @nogc nothrow @trusted
    {
        BitStorageRotate s;
        s.lengthBits = size;
        auto nw = size / U64_BITS + (size % U64_BITS != 0 ? 1 : 0);
        s.words = allocWordsOnes(nw);
        return s;
    }

    pragma(inline, true)
    void resetFlags(size_t skip) @nogc nothrow @trusted
    {
        auto start = squareStart(skip);
        auto totalBits = words.length * U64_BITS;

        auto rollBits = cast(uint)skip;
        ulong rollingMask1 = ~(1UL << (start % U64_BITS));
        ulong rollingMask2 = ~(1UL << ((start + skip) % U64_BITS));

        auto i = start;

        if (skip > U64_BITS)
        {
            auto rollBitsDouble = rollBits * 2;
            auto unrolledEnd = (totalBits >= skip) ? totalBits - skip : 0;
            while (i < unrolledEnd)
            {
                auto wordIdx1 = i / U64_BITS;
                auto wordIdx2 = (i + skip) / U64_BITS;
                words[wordIdx1] &= rollingMask1;
                words[wordIdx2] &= rollingMask2;
                rollingMask1 = rol(rollingMask1, rollBitsDouble & 63);
                rollingMask2 = rol(rollingMask2, rollBitsDouble & 63);
                i += skip * 2;
            }
        }

        while (i < totalBits)
        {
            auto wordIdx = i / U64_BITS;
            words[wordIdx] &= rollingMask1;
            i += skip;
            rollingMask1 = rol(rollingMask1, rollBits & 63);
        }
    }

    pragma(inline, true)
    bool get(size_t index) const @nogc nothrow @safe
    {
        if (index >= lengthBits) return false;
        return (words[index / U64_BITS] & (1UL << (index % U64_BITS))) != 0;
    }
}

// ---------------------------------------------------------------------------
// StripedBlocks — block-based striped storage with optional hybrid dense reset
// ---------------------------------------------------------------------------
struct StripedBlocks(size_t BLOCK_SIZE, bool HYBRID)
{
    enum BLOCK_SIZE_BITS = BLOCK_SIZE * U8_BITS;

    ubyte[] arena;
    size_t lengthBits;

    static StripedBlocks createTrue(size_t size) @nogc nothrow @trusted
    {
        StripedBlocks s;
        s.lengthBits = size;
        auto nb = size / BLOCK_SIZE_BITS + (size % BLOCK_SIZE_BITS != 0 ? 1 : 0);
        s.arena = allocBytesSet(nb * BLOCK_SIZE);
        return s;
    }

    pragma(inline, false)
    void resetDense(size_t SKIP)() @nogc nothrow @trusted
    {
        auto start = SKIP / 2 + SKIP;
        auto numBlocks = arena.length / BLOCK_SIZE;

        foreach (blockIdx; 0 .. numBlocks)
        {
            auto block = arena[blockIdx * BLOCK_SIZE .. (blockIdx + 1) * BLOCK_SIZE];
            auto preservedMask = (blockIdx == 0) ? (block[SKIP / 2] & 1) : 0;

            ubyte[8][SKIP] maskSet;

            auto startMod = start % SKIP;
            foreach (wordIdx; 0 .. SKIP)
            {
                foreach (bit; 0 .. 8)
                {
                    auto blockIndexOffset = blockIdx * BLOCK_SIZE * U8_BITS;
                    auto bitIndexOffset = bit * BLOCK_SIZE;
                    auto index = blockIndexOffset + bitIndexOffset + wordIdx;
                    ubyte mask = 0xFF;
                    if (index % SKIP == startMod)
                        mask &= ~(cast(ubyte)1 << bit);
                    maskSet[wordIdx][bit] = mask;
                }
            }

            size_t ci = 0;
            while (ci + SKIP <= block.length)
            {
                foreach (wi; 0 .. SKIP)
                {
                    auto w = block[ci + wi];
                    foreach (b; 0 .. 8)
                        w &= maskSet[wi][b];
                    block[ci + wi] = w;
                }
                ci += SKIP;
            }

            if (ci < block.length)
            {
                foreach (wi; 0 .. SKIP)
                {
                    if (ci + wi >= block.length) break;
                    auto w = block[ci + wi];
                    foreach (b; 0 .. 8)
                        w &= maskSet[wi][b];
                    block[ci + wi] = w;
                }
            }

            if (blockIdx == 0)
                block[SKIP / 2] |= preservedMask;
        }
    }

    pragma(inline, false)
    void resetGeneral(size_t skip) @nogc nothrow @trusted
    {
        auto start = squareStart(skip);
        auto numBlocks = arena.length / BLOCK_SIZE;
        auto blockIdxStart = start / BLOCK_SIZE_BITS;
        auto offsetIdx = start % BLOCK_SIZE_BITS;
        auto bitIdx = offsetIdx / BLOCK_SIZE;
        auto wordIdx = offsetIdx % BLOCK_SIZE;

        for (auto blockIdx = blockIdxStart; blockIdx < numBlocks; blockIdx++)
        {
            auto block = arena[blockIdx * BLOCK_SIZE .. (blockIdx + 1) * BLOCK_SIZE];
            while (bitIdx < U8_BITS)
            {
                auto stripeStartPos = blockIdx * BLOCK_SIZE_BITS + bitIdx * BLOCK_SIZE;
                auto effectiveLen = BLOCK_SIZE;
                if (stripeStartPos + effectiveLen > lengthBits)
                    effectiveLen = lengthBits - stripeStartPos;

                auto mask = cast(ubyte)~(1 << bitIdx);

                auto endUnrolled = (wordIdx + skip * 3 <= effectiveLen) ? effectiveLen - skip * 3 : 0;
                while (wordIdx < endUnrolled)
                {
                    block[wordIdx] &= mask;
                    block[wordIdx + skip] &= mask;
                    block[wordIdx + skip * 2] &= mask;
                    block[wordIdx + skip * 3] &= mask;
                    wordIdx += skip * 4;
                }

                while (wordIdx < effectiveLen)
                {
                    block[wordIdx] &= mask;
                    wordIdx += skip;
                }

                if (effectiveLen != BLOCK_SIZE)
                    return;

                bitIdx++;
                wordIdx -= BLOCK_SIZE;
            }

            bitIdx = 0;
        }
    }

    pragma(inline, true)
    void resetFlags(size_t skip) @nogc nothrow @trusted
    {
        static if (HYBRID)
        {
            if (skip < 9)
            {
                switch (skip)
                {
                    case 3:  resetDense!3();  return;
                    case 5:  resetDense!5();  return;
                    case 7:  resetDense!7();  return;
                    default: break;
                }
            }
        }
        resetGeneral(skip);
    }

    pragma(inline, true)
    bool get(size_t index) const @nogc nothrow @safe
    {
        if (index > lengthBits) return false;
        auto block = index / BLOCK_SIZE_BITS;
        auto offset = index % BLOCK_SIZE_BITS;
        auto bitIdx2 = offset / BLOCK_SIZE;
        auto wordIdx2 = offset % BLOCK_SIZE;
        return (arena[block * BLOCK_SIZE + wordIdx2] & (1 << bitIdx2)) != 0;
    }
}

// HACK: need to store lengthBits for StripedBlocks.get()
// We store it in a hacky way - the last 8 bytes of arena
// Actually no, let me just add a separate field
// Since StripedBlocks is a template struct, I need a different approach
// Let me use a simple wrapper

// ---------------------------------------------------------------------------
// CTFE: compute combined masks for extreme approach (grouped by word)
// ---------------------------------------------------------------------------
pure nothrow @nogc ulong[SKIP] computeCombinedMasks64(int SKIP)()
{
    ulong[SKIP] masks;
    masks[] = 0;
    auto start = SKIP / 2;
    foreach (bit; 0 .. 64)
    {
        auto idx = start + bit * SKIP;
        auto wordIdx = idx / 64;
        if (wordIdx < SKIP)
            masks[wordIdx] |= (1UL << (idx % 64));
    }
    return masks;
}

// ---------------------------------------------------------------------------
// Extreme dense reset — groups ORs by word (one load-modify-store per word)
// ---------------------------------------------------------------------------
pragma(inline, false)
void resetDenseExtreme(int SKIP)(ulong[] words) @nogc nothrow @trusted
{
    enum combinedMasks = computeCombinedMasks64!SKIP();

    auto start = squareStart(SKIP);
    auto startChunk = start / 64 / SKIP * SKIP;

    size_t ci = startChunk;
    while (ci + SKIP <= words.length)
    {
        static foreach (wi; 0 .. SKIP)
        {
            static if (combinedMasks[wi])
                words[ci + wi] |= combinedMasks[wi];
        }
        ci += SKIP;
    }

    // remainder
    if (ci < words.length)
    {
        static foreach (wi; 0 .. SKIP)
        {
            static if (combinedMasks[wi])
            {
                if (ci + wi < words.length)
                    words[ci + wi] |= combinedMasks[wi];
            }
        }
    }

    auto factorIdx = SKIP / 2;
    auto factorWord = factorIdx / 64;
    auto factorBit = factorIdx % 64;
    if (factorWord < words.length)
        words[factorWord] &= ~(1UL << factorBit);
}

// ---------------------------------------------------------------------------
// UnrolledHybrid — linear u64 storage with dense/sparse hybrid reset
// ---------------------------------------------------------------------------
struct UnrolledHybrid
{
    ulong[] words;
    size_t lengthBits;

    static UnrolledHybrid createTrue(size_t size) @nogc nothrow @trusted
    {
        UnrolledHybrid s;
        s.lengthBits = size;
        auto nw = size / U64_BITS + (size % U64_BITS != 0 ? 1 : 0);
        s.words = allocWords(nw);
        return s;
    }

    pragma(inline, true)
    void resetFlags(size_t skip) @nogc nothrow @trusted
    {
        if (skip <= 129)
        {
            mixin(genDenseDispatch("resetDense", "words"));
            return;
        }

        auto equivSkip = patternEquivalentSkip(skip, 8);
        mixin(genSparseDispatch("resetSparse", "words, skip"));
    }

    pragma(inline, true)
    bool get(size_t index) const @nogc nothrow @safe
    {
        if (index >= lengthBits) return false;
        return (words[index / U64_BITS] & (1UL << (index % U64_BITS))) == 0;
    }
}

// ---------------------------------------------------------------------------
// ExtremeHybrid — like UnrolledHybrid but uses word-grouped extreme dense reset
// ---------------------------------------------------------------------------
struct ExtremeHybrid
{
    ulong[] words;
    size_t lengthBits;

    static ExtremeHybrid createTrue(size_t size) @nogc nothrow @trusted
    {
        ExtremeHybrid s;
        s.lengthBits = size;
        auto nw = size / U64_BITS + (size % U64_BITS != 0 ? 1 : 0);
        s.words = allocWords(nw);
        return s;
    }

    pragma(inline, true)
    void resetFlags(size_t skip) @nogc nothrow @trusted
    {
        if (skip <= 129)
        {
            mixin(genDenseDispatch("resetDenseExtreme", "words"));
            return;
        }

        auto equivSkip = patternEquivalentSkip(skip, 8);
        mixin(genSparseDispatch("resetSparse", "words, skip"));
    }

    pragma(inline, true)
    bool get(size_t index) const @nogc nothrow @safe
    {
        if (index >= lengthBits) return false;
        return (words[index / U64_BITS] & (1UL << (index % U64_BITS))) == 0;
    }
}

// ---------------------------------------------------------------------------
// PrimeSieve — generic over storage type
// ---------------------------------------------------------------------------
struct PrimeSieve(T)
{
    size_t sieveSize;
    T flags;

    static PrimeSieve create(size_t size) @nogc nothrow @trusted
    {
        PrimeSieve s;
        s.sieveSize = size;
        auto numFlags = size / 2 + 1;
        s.flags = T.createTrue(numFlags);
        return s;
    }

    pragma(inline, true)
    bool isNumFlagged(size_t number) const @nogc nothrow @safe
    {
        if (number % 2 == 0) return false;
        return flags.get(number / 2);
    }

    size_t countPrimes() const @nogc nothrow @safe
    {
        size_t count = 0;
        for (size_t i = 1; i < sieveSize; i++)
        {
            if (isNumFlagged(i))
                count++;
        }
        return count;
    }

    void runSieve() @nogc nothrow @trusted
    {
        size_t factor = 3;
        auto q = cast(size_t)sqrt(cast(double)sieveSize);

        while (true)
        {
            size_t n;
            for (n = factor / 2; n <= sieveSize / 2; n++)
            {
                if (flags.get(n))
                {
                    factor = n * 2 + 1;
                    break;
                }
            }

            if (factor > q)
                break;

            flags.resetFlags(factor);
            factor += 2;
        }
    }
}

// ---------------------------------------------------------------------------
// Hacked StripedBlocks with lengthBits stored separately
// ---------------------------------------------------------------------------
// We need to store lengthBits for StripedBlocks. The struct template already
// has it defined as a member in the code above, but I used it in resetGeneral
// without defining it. Let me fix this by creating a wrapper.

struct StripedBlocksWithLength(T)
{
    T impl;
    size_t lengthBits;

    static StripedBlocksWithLength createTrue(size_t size) @nogc nothrow @trusted
    {
        StripedBlocksWithLength s;
        s.impl = T.createTrue(size);
        s.lengthBits = size / 2 + 1; // num flags
        return s;
    }

    pragma(inline, true)
    void resetFlags(size_t skip) @nogc nothrow @trusted
    {
        impl.resetFlags(skip);
    }

    pragma(inline, true)
    bool get(size_t index) const @nogc nothrow @safe
    {
        // delegate to impl's get which uses the pre-computed lengthBits
        // But impl doesn't have lengthBits. Hmm, it's stored in the arena.
        // Actually, looking at the use of lengthBits in resetGeneral, it's
        // the total number of flags. Let me just compute it from the arena size.
        return impl.get(index);
    }
}

// ---------------------------------------------------------------------------
// Output helpers
// ---------------------------------------------------------------------------
void printResultsStderr(T)(string label, ref const PrimeSieve!T sieve,
    bool showResults, Duration duration, size_t passes, size_t threads,
    ref const PrimeValidator validator) @trusted
{
    if (showResults)
    {
        stderr.write("2,");
        for (auto num = 3; num < sieve.sieveSize; num++)
        {
            if (sieve.isNumFlagged(num))
                stdout.write(num, ",");
        }
        stderr.writeln();
    }

    auto count = sieve.countPrimes();
    auto secs = duration.total!"hnsecs"() / 1e7;
    auto passesPerSec = passes / secs;
    auto valid = validator.isValid(sieve.sieveSize, count);

    stderr.writefln!"%30s Passes: %s, Threads: %s, Time: %.10f, Passes / sec: %.2f, Limit: %s, Counts: %s, Valid: %s"(
        label, passes, threads, secs, passesPerSec, sieve.sieveSize, count,
        valid ? "Pass" : "Fail");
}

void reportResultsStdout(string label, size_t bitsPerPrime,
    Duration duration, size_t passes, size_t threads)
{
    auto secs = duration.total!"hnsecs"() / 1e7;
    writefln!"serg-gini_%s;%s;%.10f;%s;algorithm=base,faithful=yes,bits=%s"(
        label, passes, secs, threads, bitsPerPrime);
}

// ---------------------------------------------------------------------------
// CL options
// ---------------------------------------------------------------------------
struct CliOptions
{
    size_t threads;
    size_t seconds = 5;
    size_t limit = 1000000;
    size_t repetitions = 1;
    bool print;
    bool bits;
    bool bitsRotate;
    bool bitsStriped;
    bool bitsStripedBlocks;
    bool bitsStripedHybrid;
    bool bitsUnrolled;
    bool bitsExtreme;
    bool bytes;
}

// ---------------------------------------------------------------------------
// Thread helpers
// ---------------------------------------------------------------------------
size_t[] getAutoThreadsList(size_t logicalCores) @safe
{
    size_t[] vals;
    if (logicalCores >= 64)
        vals = [1, 4, logicalCores / 2, logicalCores];
    else if (logicalCores >= 32)
        vals = [1, 4, 16, 32];
    else if (logicalCores >= 16)
        vals = [1, 4, 8, 16];
    else if (logicalCores >= 12)
        vals = [1, 4, 6, 12];
    else if (logicalCores >= 10)
        vals = [1, 4, 5, 10];
    else if (logicalCores >= 8)
        vals = [1, 4, 8];
    else if (logicalCores >= 6)
        vals = [1, 3, 4, 6];
    else if (logicalCores >= 4)
        vals = [1, 2, 4];
    else if (logicalCores >= 3)
        vals = [1, 3];
    else
        vals = [1, 2];

    auto result = vals.filter!(v => v > 0 && v <= logicalCores).array;
    result.sort;
    size_t j = 0;
    foreach (i; 0 .. result.length)
    {
        if (j == 0 || result[i] != result[j - 1])
        {
            result[j] = result[i];
            j++;
        }
    }
    return result[0 .. j];
}

// ---------------------------------------------------------------------------
// Runners
// ---------------------------------------------------------------------------
void runImplementationST(T)(string label, size_t bitsPerPrime,
    Duration runDuration, size_t limit, bool printPrimes) @trusted
{
    auto startTime = MonoTime.currTime;
    size_t localPasses;
    PrimeSieve!T lastSieve;

    while (true)
    {
        auto elapsed = MonoTime.currTime - startTime;
        if (elapsed >= runDuration) break;

        auto sieve = PrimeSieve!T.create(limit);
        sieve.runSieve();
        lastSieve = sieve;
        localPasses++;
    }

    auto endTime = MonoTime.currTime;
    auto duration = endTime - startTime;

    auto validator = PrimeValidator();
    printResultsStderr(label, lastSieve, printPrimes, duration, localPasses, 1, validator);
    reportResultsStdout(label, bitsPerPrime, duration, localPasses, 1);
    stderr.writeln();
}

struct ThreadResult
{
    size_t passes;
    size_t sieveSize;
    size_t primeCount;
}

auto makeThreadFunc(T)(size_t idx, ThreadResult* result, MonoTime startTime,
    Duration runDuration, size_t limit) @trusted
{
    return delegate void() @trusted {
        size_t passes;
        PrimeSieve!T lastSieve;

        while (true)
        {
            auto elapsed = MonoTime.currTime - startTime;
            if (elapsed >= runDuration) break;

            auto sieve = PrimeSieve!T.create(limit);
            sieve.runSieve();
            lastSieve = sieve;
            passes++;
        }

        *result = ThreadResult(passes, lastSieve.sieveSize, lastSieve.countPrimes());
    };
}

void runImplementationMT(T)(string label, size_t bitsPerPrime,
    Duration runDuration, size_t numThreads, size_t limit, bool printPrimes) @trusted
{
    import core.thread : Thread;

    auto results = new ThreadResult[numThreads];

    auto startTime = MonoTime.currTime;

    auto threads = new Thread[numThreads];
    foreach (i; 0 .. numThreads)
    {
        auto idx = i;
        threads[i] = new Thread(makeThreadFunc!T(idx, &results[i], startTime, runDuration, limit));
    }

    foreach (t; threads) t.start();
    foreach (t; threads) t.join();

    auto endTime = MonoTime.currTime;
    auto duration = endTime - startTime;

    size_t totalPasses;
    size_t totalSieveSize;
    size_t totalPrimeCount;
    foreach (r; results)
    {
        totalPasses += r.passes;
        if (r.sieveSize > 0)
        {
            totalSieveSize = r.sieveSize;
            totalPrimeCount = r.primeCount;
        }
    }

    auto validator = PrimeValidator();
    auto valid = validator.isValid(totalSieveSize, totalPrimeCount);

    stderr.writefln!"%30s Passes: %s, Threads: %s, Time: %.10f, Passes / sec: %.2f, Limit: %s, Counts: %s, Valid: %s"(
        label, totalPasses, numThreads, duration.total!"hnsecs"() / 1e7,
        totalPasses / (duration.total!"hnsecs"() / 1e7),
        totalSieveSize, totalPrimeCount,
        valid ? "Pass" : "Fail");

    reportResultsStdout(label, bitsPerPrime, duration, totalPasses, numThreads);
    stderr.writeln();
}

void runImplementation(T)(string label, size_t bitsPerPrime,
    Duration runDuration, size_t numThreads, size_t limit,
    bool printPrimes, size_t repetitions) @trusted
{
    foreach (_; 0 .. repetitions)
    {
        auto coolStart = MonoTime.currTime;
        while ((MonoTime.currTime - coolStart) < dur!"seconds"(5)) {}

        if (numThreads == 1)
            runImplementationST!(T)(label, bitsPerPrime, runDuration, limit, printPrimes);
        else
            runImplementationMT!(T)(label, bitsPerPrime, runDuration, numThreads, limit, printPrimes);
    }
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------
int main(string[] args)
{
    CliOptions opt;

    auto helpInfo = getopt(args,
        "threads|t",      &opt.threads,
        "seconds|s",      &opt.seconds,
        "limit|l",        &opt.limit,
        "repetitions|r",  &opt.repetitions,
        "print|p",        &opt.print,
        "bits",           &opt.bits,
        "bits-rotate",    &opt.bitsRotate,
        "bits-striped",   &opt.bitsStriped,
        "bits-striped-blocks", &opt.bitsStripedBlocks,
        "bits-striped-hybrid", &opt.bitsStripedHybrid,
        "bits-unrolled",  &opt.bitsUnrolled,
        "bits-extreme",   &opt.bitsExtreme,
        "bytes",          &opt.bytes,
    );

    if (helpInfo.helpWanted)
    {
        stderr.writefln("Usage: %s [options]\n\nOptions:", args[0]);
        stderr.writeln("  -t/--threads       Number of threads");
        stderr.writeln("  -s/--seconds       Run duration (default: 5)");
        stderr.writeln("  -l/--limit         Sieve limit (default: 1000000)");
        stderr.writeln("  -r/--repetitions   Number of repetitions (default: 1)");
        stderr.writeln("  -p/--print         Print all primes found");
        stderr.writeln("  --bits             Use bit-level storage");
        stderr.writeln("  --bits-rotate      Use bit-level storage with rotate");
        stderr.writeln("  --bits-striped     Use striped bit storage");
        stderr.writeln("  --bits-striped-blocks  Use striped blocks storage");
        stderr.writeln("  --bits-striped-hybrid  Use striped blocks hybrid storage");
        stderr.writeln("  --bits-unrolled    Use unrolled hybrid storage");
        stderr.writeln("  --bits-extreme     Use extreme hybrid storage");
        stderr.writeln("  --bytes            Use byte-level storage");
        return 0;
    }

    auto limit = opt.limit;
    auto repetitions = opt.repetitions;
    auto runDuration = dur!"seconds"(opt.seconds);

    auto threadOptions = (opt.threads > 0)
        ? [opt.threads]
        : getAutoThreadsList(8);

    auto runAll = !opt.bits && !opt.bitsRotate && !opt.bitsStriped &&
        !opt.bitsStripedBlocks && !opt.bitsStripedHybrid &&
        !opt.bitsUnrolled && !opt.bitsExtreme && !opt.bytes;

    foreach (threads; threadOptions)
    {
        stderr.writeln("\n-------------------------------------------------------");
        stderr.writefln("Computing primes to %s on %s thread%s for %s second%s.",
            limit, threads, threads == 1 ? "" : "s",
            runDuration.total!"seconds",
            runDuration.total!"seconds" == 1 ? "" : "s");
        stderr.writeln("-------------------------------------------------------\n");

        if (opt.bytes)
            runImplementation!(ByteStorage)("byte", 8, runDuration, threads, limit, opt.print, repetitions);

        if (opt.bits)
            runImplementation!(BitStorage)("bit", 1, runDuration, threads, limit, opt.print, repetitions);

        if (opt.bitsRotate || runAll)
            runImplementation!(BitStorageRotate)("bit-rotate", 1, runDuration, threads, limit, opt.print, repetitions);

        if (opt.bitsStriped)
            runImplementation!(StripedBlocks!(BLOCK_SIZE_DEFAULT, false))("bit-striped", 1, runDuration, threads, limit, opt.print, repetitions);

        if (opt.bitsStripedBlocks)
        {
            runImplementation!(StripedBlocks!(BLOCK_SIZE_DEFAULT, false))("bit-striped-blocks16k", 1, runDuration, threads, limit, opt.print, repetitions);
            runImplementation!(StripedBlocks!(BLOCK_SIZE_SMALL, false))("bit-striped-blocks4k", 1, runDuration, threads, limit, opt.print, repetitions);
        }

        if (opt.bitsStripedHybrid)
        {
            runImplementation!(StripedBlocks!(BLOCK_SIZE_DEFAULT, true))("bit-striped-hybrid-blocks16k", 1, runDuration, threads, limit, opt.print, repetitions);
            runImplementation!(StripedBlocks!(BLOCK_SIZE_SMALL, true))("bit-striped-hybrid-blocks4k", 1, runDuration, threads, limit, opt.print, repetitions);
        }

        if (opt.bitsUnrolled || runAll)
            runImplementation!(UnrolledHybrid)("bit-unrolled-hybrid", 1, runDuration, threads, limit, opt.print, repetitions);

        if (opt.bitsExtreme || runAll)
            runImplementation!(ExtremeHybrid)("bit-extreme-hybrid", 1, runDuration, threads, limit, opt.print, repetitions);
    }

    return 0;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------
version (unittest)
{
    import std.algorithm : equal;

    @safe unittest
    {
        auto validator = PrimeValidator();
        assert(validator.isValid(10, 4));
        assert(validator.isValid(100, 25));
        assert(validator.isValid(1000000, 78498));
        assert(!validator.isValid(1000000, 0));
    }

    @safe unittest
    {
        assert(squareStart(3) == 4);
        assert(squareStart(5) == 12);
        assert(squareStart(7) == 24);
    }

    @safe unittest
    {
        assert(minimumStart(3) == 4);
        assert(minimumStart(5) == 7);
        assert(minimumStart(7) == 10);
    }

    void testStorage(T)() @trusted
    {
        auto size = size_t(100_000);
        auto storage = T.createTrue(size);

        foreach (i; 0 .. size)
            assert(storage.get(i), "expected initially true for index " ~ i.to!string);

        storage.resetFlags(5);
        auto s5 = squareStart(5);
        auto m5 = minimumStart(5);
        foreach (i; 0 .. size)
        {
            if (i >= m5 && i <= s5 && (i - m5) % 5 == 0)
                continue;
            auto expectedInv = (i >= s5) && ((i - s5) % 5 == 0);
            assert(storage.get(i) == !expectedInv,
                "expected value incorrect for index " ~ i.to!string);
        }

        storage.resetFlags(13);
        auto s13 = squareStart(13);
        auto m13 = minimumStart(13);
        foreach (i; 0 .. size)
        {
            if (i >= m5 && i <= s5 && (i - m5) % 5 == 0)
                continue;
            if (i >= m13 && i <= s13 && (i - m13) % 13 == 0)
                continue;
            auto first = (i >= s5) && ((i - s5) % 5 == 0);
            auto second = (i >= s13) && ((i - s13) % 13 == 0);
            auto expectedInv = first || second;
            assert(storage.get(i) == !expectedInv,
                "expected value incorrect for index " ~ i.to!string);
        }
    }

    unittest { testStorage!ByteStorage(); }
    unittest { testStorage!BitStorage(); }
    unittest { testStorage!BitStorageRotate(); }
    unittest { testStorage!(StripedBlocks!(BLOCK_SIZE_DEFAULT, false))(); }
    unittest { testStorage!(StripedBlocks!(BLOCK_SIZE_SMALL, false))(); }
    unittest { testStorage!(StripedBlocks!(BLOCK_SIZE_DEFAULT, true))(); }
    unittest { testStorage!(StripedBlocks!(BLOCK_SIZE_SMALL, true))(); }
    unittest { testStorage!(StripedBlocks!(7, false))(); }
    unittest { testStorage!(StripedBlocks!(1024, false))(); }
    unittest { testStorage!(StripedBlocks!(7, true))(); }
    unittest { testStorage!(StripedBlocks!(1024, true))(); }
    unittest { testStorage!UnrolledHybrid(); }
    unittest { testStorage!ExtremeHybrid(); }

    void sieveKnownCorrect(T)() @trusted
    {
        auto validator = PrimeValidator();
        foreach (i, ref lim; validator.limits)
        {
            if (lim > 10_000_000) continue;
            auto sieve = PrimeSieve!T.create(lim);
            sieve.runSieve();
            auto count = sieve.countPrimes();
            assert(count == validator.counts[i],
                "wrong primes for sieve=" ~ lim.to!string ~
                " expected=" ~ validator.counts[i].to!string ~
                " got=" ~ count.to!string);
        }
    }

    unittest { sieveKnownCorrect!ByteStorage(); }
    unittest { sieveKnownCorrect!BitStorage(); }
    unittest { sieveKnownCorrect!BitStorageRotate(); }
    unittest { sieveKnownCorrect!(StripedBlocks!(BLOCK_SIZE_DEFAULT, false))(); }
    unittest { sieveKnownCorrect!(StripedBlocks!(BLOCK_SIZE_SMALL, false))(); }
    unittest { sieveKnownCorrect!(StripedBlocks!(BLOCK_SIZE_DEFAULT, true))(); }
    unittest { sieveKnownCorrect!(StripedBlocks!(BLOCK_SIZE_SMALL, true))(); }
    unittest { sieveKnownCorrect!UnrolledHybrid(); }
    unittest { sieveKnownCorrect!ExtremeHybrid(); }
}
