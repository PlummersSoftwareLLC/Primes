using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Text;

namespace PrimeSieveCS
{
    readonly struct SieveStride8Blocks16kRunner : ISieveRunner
    {
        public ISieve RunSieve(uint sieveSize) => new SieveStride8Blocks(sieveSize, 0x4000, "italytoast-stride8-blocks16k").RunSieve();
    }

    readonly struct SieveStride8Blocks32kRunner : ISieveRunner
    {
        public ISieve RunSieve(uint sieveSize) => new SieveStride8Blocks(sieveSize, 0x8000, "italytoast-stride8-blocks32k").RunSieve();
    }

    readonly struct SieveStride8Blocks64kRunner : ISieveRunner
    {
        public ISieve RunSieve(uint sieveSize) => new SieveStride8Blocks(sieveSize, 0x10000, "italytoast-stride8-blocks64k").RunSieve();
    }

    /// <summary>
    /// A simplified version of the striped algorithm used by rust solution_1.
    /// Instead of having the sieve oriented "vertically", we keep the normal bit array representaion.
    /// What we do instead is create the mask and push the pointer by factor. 
    /// That way we effectively mark every 8th factor. 
    /// 
    /// This simplifies the code a whole lot.
    /// </summary>
    class SieveStride8Blocks : ISieve
    {
        readonly uint sieveSize = 0;
        readonly uint halfLimit;
        readonly ulong[] bits;
        readonly uint blockSize;
        readonly string name;

        public string Name => name;

        public uint SieveSize => sieveSize;

        public SieveStride8Blocks(uint size, uint blocksize, string name)
        {
            const int wordBits = sizeof(ulong) * 8;

            this.name = name;
            blockSize = blocksize;
            sieveSize = size;
            halfLimit = (size + 1) / 2;
            bits = new ulong[(int)(halfLimit / wordBits + 1)];
        }

        public IEnumerable<uint> EnumeratePrimes()
        {
            yield return 2;
            for (uint num = 3; num <= sieveSize; num += 2)
                if ((bits[(num / 2) / 64] & (1UL << (int)(num / 2))) == 0)
                    yield return num;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        unsafe static void ClearBitsStride8BlocksUnrolled(byte* ptr, uint start, uint factor, uint limit, uint blocksize)
        {
            Span<(uint, byte)> strides = stackalloc (uint, byte)[8];
            for (uint i = 0; i < 8; i++)
            {
                var s = start + factor * i;
                strides[(int)i] = (s / 8, (byte)(1 << ((int)s % 8)));
            }

            var bytecount = limit / 8;
            var blockStart = start / 8;

            while (blockStart <= bytecount)
            {
                for (int stride = 0; stride < 8; stride++)
                {
                    var (index, mask) = strides[stride];
                    var blockEnd = Math.Min(bytecount + 1, index + blocksize);
                    var blockEndPtr = ptr + blockEnd;

                    var i0 = ptr + index;
                    var i1 = ptr + index + factor;
                    var i2 = ptr + index + factor * 2;
                    var i3 = ptr + index + factor * 3;

                    uint factor4 = factor * 4; 
                    for (; i3 < blockEndPtr;)
                    {
                        i0[0] |= mask;
                        i1[0] |= mask;
                        i2[0] |= mask;
                        i3[0] |= mask;

                        i0 += factor4;
                        i1 += factor4;
                        i2 += factor4;
                        i3 += factor4;
                    }

                    for (; i0 < blockEndPtr; i0 += factor)
                    {
                        i0[0] |= mask;
                    }

                    strides[stride] = ((uint)(i0 - ptr), mask);
                }

                blockStart += blocksize;
            }
        }

        /// <summary>
        /// Calculate the primes up to the specified limit
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveOptimization)]
        unsafe public SieveStride8Blocks RunSieve()
        {
            uint factor = 3;
            uint halfFactor = factor >> 1;
            uint halfRoot = ((uint)(Math.Sqrt(sieveSize) + 1)) >> 1;

            // We ignore even numbers by using values that track half of the actuals, and the only
            // number we keep in original form is the prime factor we're walking through the sieve
            fixed (ulong* ptr = bits)
                while (true)
                {
                    // Scan for the next unset bit which means it is a prime factor
                    var segment = ptr[halfFactor / 64];
                    var offset = halfFactor % 64;
                    segment = ~segment; //since we only have access to TrailingZeroCount, we have to flip all the bits
                    segment >>= (int)offset;
                    var jump = BitOperations.TrailingZeroCount(segment);
                    if (jump == 64)
                    {
                        halfFactor += 64 - offset;
                        continue;
                    }

                    //scan finnished - restoring factor
                    halfFactor += (uint)jump;
                    factor = (halfFactor << 1) + 1;
                    halfFactor++;

                    if (halfFactor > halfRoot) break;

                    ClearBitsStride8BlocksUnrolled((byte*)ptr, (factor * factor) / 2, factor, halfLimit, blockSize);
                }
            return this;
        }
    }
}
