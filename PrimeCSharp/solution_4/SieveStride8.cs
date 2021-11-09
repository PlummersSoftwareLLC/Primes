using System;
using System.Collections.Generic;
using System.Numerics;
using System.Runtime.CompilerServices;

namespace PrimeSieveCS
{
    readonly struct SieveStride8Runner : ISieveRunner
    {
        public ISieve RunSieve(uint sieveSize) => new SieveStride8(sieveSize).RunSieve();
    }


    /// <summary>
    /// A simplified version of the striped algorithm used by rust solution_1.
    /// Instead of having the sieve oriented "vertically", we keep the normal bit array representaion.
    /// What we do instead is create the mask and push the pointer by factor. 
    /// That way we effectively mark every 8th factor. 
    /// 
    /// This simplifies the code a whole lot.
    /// </summary>
    class SieveStride8 : ISieve
    {
        readonly uint sieveSize;
        readonly uint halfLimit;
        readonly ulong[] bits;

        public string Name => "italytoast-stride8";

        public uint SieveSize => sieveSize;

        public SieveStride8(uint size)
        {
            const int wordBits = sizeof(ulong) * 8;

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
        unsafe static void ClearBitsStriped(byte* ptr, uint start, uint factor, uint limit)
        {
            var stripedMask = 0;
            var count = limit / 8;
            while (true)
            {
                var index = start / 8;
                byte mask = (byte)(1 << (int)(start % 8));

                if ((mask & stripedMask) != 0) break;//checks if we have cleared the stripe already. If so we are finnished.

                for (uint i = index; i < count; i += factor)
                {
                    ptr[i] |= mask;
                }
                start += factor;// push start forward by factor so we start in the next stripe.
                stripedMask |= mask;
            }
        }

        /// <summary>
        /// Calculate the primes up to the specified limit
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveOptimization)]
        unsafe public SieveStride8 RunSieve()
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

                    ClearBitsStriped((byte*)ptr, (factor * factor) / 2, factor, halfLimit);
                }
            return this;
        }
    }
}
