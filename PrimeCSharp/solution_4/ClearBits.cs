using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Text;

namespace PrimeSieveCS
{
    /// <summary>
    /// Diffrent algorithms to clear bits for a factor, not actually used. 
    /// They are kept around to compare different approaches for clearing the bits.
    /// </summary>
    class ClearBits
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        unsafe public static void ClearBitsSparseRev(ulong* ptr, uint start, uint factor, uint limit)
        {
            uint bitsset = (limit - start) / factor;
            var last = start + bitsset * factor;

            uint index = last;
            for (int i = (int)bitsset; i >= 0; i--)
            {
                ptr[index / 64] |= 1UL << (int)(index % 64);
                index -= factor;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        unsafe public static void ClearBitsSparseRevUnrolled4(ulong* ptr, uint start, uint factor, uint limit)
        {
            uint bitsset = (limit - start) / factor;
            var last = start + bitsset * factor;

            int iter = (int)bitsset - 4;
            uint i0 = last;
            uint i1 = last - factor;
            uint i2 = last - factor * 2;
            uint i3 = last - factor * 3;

            var factor4 = factor * 4;
            while (iter > 0)
            {
                ptr[i0 / 64] |= 1ul << (int)(i0 % 64);
                ptr[i1 / 64] |= 1ul << (int)(i1 % 64);
                ptr[i2 / 64] |= 1ul << (int)(i2 % 64);
                ptr[i3 / 64] |= 1ul << (int)(i3 % 64);

                i0 -= factor4;
                i1 -= factor4;
                i2 -= factor4;
                i3 -= factor4;

                iter -= 4;
            }

            iter += 4;

            while (iter >= 0)
            {
                ptr[i0 / 64] |= 1ul << (int)(i0 % 64);
                i0 -= factor;

                iter--;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        unsafe public static void ClearBitsSparseUnrolled4(ulong* ptr, uint start, uint factor, uint limit)
        {
            var i0 = start;
            var i1 = start + factor;
            var i2 = start + factor * 2;
            var i3 = start + factor * 3;

            var factor4 = factor * 4;
            while (i3 < limit)
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

            while (i0 < limit)
            {
                ptr[i0 / 64] |= 1ul << (int)(i0 % 64);
                i0 += factor;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        unsafe public static void ClearBitsSparseUnrolled4V2(ulong* ptr, uint start, uint factor, uint limit)
        {
            int lim = (int)limit; // hoist and convert
            var i0 = start;
            var i1 = start + factor;
            var i2 = start + factor * 2;
            var i3 = start + factor * 3;

            var factor4 = factor * 4;
            while (i3 < lim)
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

            while (i0 < lim)
            {
                ptr[i0 / 64] |= 1ul << (int)(i0 % 64);
                i0 += factor;
            }
        }
    }
}
