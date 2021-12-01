using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;
using System.Runtime.CompilerServices;

namespace PrimeSieveCS
{
    readonly struct SieveDenseAndSparseRunner : ISieveRunner
    {
        public ISieve RunSieve(uint sieveSize) => new SieveDenseAndSparse(sieveSize).RunSieve();
    }

    class SieveDenseAndSparse : ISieve
    {
        readonly uint sieveSize;
        readonly uint halfLimit;
        readonly ulong[] bits;

        public string Name => "italytoast-dense-and-sparse";

        public uint SieveSize => sieveSize;

        public SieveDenseAndSparse(uint size)
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

        /// <summary>
        /// A clear bits function thats using pointers so we dont need to store the index in a register.
        /// 
        /// We do need to do a sub for each comparison in the outer loop. 
        /// There might be a faster version we can make with an index if we reverse it.
        /// </summary>
        unsafe static void ClearBitsDense(ulong* ptr, uint start, int factor, uint limit)
        {
            Debug.Assert(factor < 64, "factor cant be bigger than 63, that will cause incorrect calcualtions. This is optimized for lower factors");

            //Performance: we want factor and offset as an int so we can dodge a SUB in the while comparison in the inner loop

            var ptrStart = ptr + start / 64;
            var ptrEnd = ptr + limit / 64;

            ulong rollingMask = 1UL << (int)(start);
            int offset = (int)(64 - start % 64);
            while (ptrStart <= ptrEnd)
            {
                var segment = ptrStart[0];
                do
                {
                    segment |= rollingMask;
                    rollingMask = BitOperations.RotateLeft(rollingMask, factor);
                    offset -= factor;
                } while (offset > 0);
                offset += 64;
                ptrStart[0] = segment;
                ptrStart++;
            }
        }

        //Note: this is not actually used
        //it is the reference for the unrolled version: ClearBitsSparseUnrolled4Rev
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        unsafe static void ClearBitsSparse(ulong* ptr, uint start, uint factor, uint limit)
        {
            for (uint index = start; index < limit; index += factor)
            {
                ptr[index / 64] |= 1UL << (int)(index % 64);
            }
        }

        /// <summary>
        /// Unrolled version of ClearBitsSparse.
        /// 
        /// Provided by mike-barber. Reversed version by ItalyToast
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        unsafe static void ClearBitsSparseUnrolled4Rev(ulong* ptr, uint start, uint factor, uint limit)
        {
            uint bitsset = (limit - start) / factor;
            int iter = (int)bitsset;

            var i0 = start;
            var i1 = start + factor;
            var i2 = start + factor * 2;
            var i3 = start + factor * 3;

            var factor4 = factor * 4;
            for (iter -= 4; iter > 0; iter -= 4)
            {
                ptr[i0 / 64] |= 1ul << (int)(i0 % 64);
                ptr[i1 / 64] |= 1ul << (int)(i1 % 64);
                ptr[i2 / 64] |= 1ul << (int)(i2 % 64);
                ptr[i3 / 64] |= 1ul << (int)(i3 % 64);

                i0 += factor4;
                i1 += factor4;
                i2 += factor4;
                i3 += factor4;
            }

            for (iter += 4; iter >= 0; iter--)
            {
                ptr[i0 / 64] |= 1ul << (int)(i0 % 64);
                i0 += factor;
            }
        }

        /// <summary>
        /// Calculate the primes up to the specified limit
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveOptimization)]
        unsafe public SieveDenseAndSparse RunSieve()
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

                    //marking with a rolling mask if we can get enough bits in the ulong.
                    //Half factor of 20 seems to be optimal. (~3 bits / ulong) 
                    if (halfFactor < 20)
                    {
                        ClearBitsDense(ptr, (factor * factor) / 2, (int)factor, halfLimit);
                    }
                    else
                    {
                        ClearBitsSparseUnrolled4Rev(ptr, (factor * factor) / 2, factor, halfLimit);
                    }
                }
            return this;
        }
    }
}
