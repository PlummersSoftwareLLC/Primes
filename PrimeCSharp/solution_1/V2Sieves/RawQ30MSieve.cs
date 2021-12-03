using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace PrimeCSharp.V2Sieves
{
    public class RawQ30MSieve : ISieveRunner
    {
        public string Name => "rawq30m";
        public string Description => "Raw data, 64 bit, 8 of 30, bitmasking";
        public string AlgorithmType => "other";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly ulong[] data;
        private const int dataBits = 64;
        private const int dataBitsShift = 6;
        private const int dataBitsMask = 63;
        private const int PowerScale = 6; // 2^6 = 64 == BitsPerWord
        private const int MaskScale = 1; // 0 for keeping all values, 1 for only odd numbers, etc
        private const int IndexScale = PowerScale + MaskScale;

        public RawQ30MSieve(int sieveSize)
        {
            if (sieveSize < 0)
                throw new ArgumentOutOfRangeException(nameof(sieveSize));

            SieveSize = sieveSize;

            data = GC.AllocateUninitializedArray<ulong>((SieveSize >> (dataBitsShift + 1)) + 1, pinned: true);
            data.AsSpan().Fill(0);
        }

        const int stepsLength = 8;

        // Only numbers congruent to candidates mod 30 can be prime.
        // Only for informative purposes
        //int[] candidates = new int[stepsLength] { 1, 7, 11, 13, 17, 19, 23, 29 };

        // Steps are the distances to the next candidate
        readonly int[] steps = new int[stepsLength] { 6, 4, 2, 4, 2, 4, 6, 2 };

        public void Run()
        {
            int q = (int)Math.Sqrt(SieveSize);

            // We can skip 2, 3, and 5, and start our checks with 7.
            // This will be index 1 in the steps list.

            // Cached bitmaps and index offsets for bit twiddling loop.
            ulong[] masks = new ulong[dataBits];
            int[] offsets = new int[dataBits];

            // Look for next prime
            for (int factor = 7, step = 1, inc = steps[step];
                factor <= q;
                factor += inc, step = (step + 1) & 7, inc = steps[step])
            {
                // A set bit means it's composite - keep searching.
                if (GetBit(factor))
                {
                    continue;
                }

                // The following loop is the hotspot for this algorithm.
                // No need to start less than p^2 since all those
                // multiples have already been marked.

                // Performance optimization: since the bit mask we `or`
                // into the word (and the index offset added to the base)
                // RECUR every 64 iterations of this loop (for 64-bit words), we
                // can precalculate them and use them over and over until the end
                // of the sieve array.

                int baseVal = IndexOf(factor * factor);
                int cumOffset = 0;
                masks.AsSpan().Fill(0);
                int iUsed = 0;
                int offset = 0;

                for (int bitCount = 0, m = factor * factor; bitCount < dataBits; bitCount++, m += factor * 2)
                {
                    masks[iUsed] |= MaskOf(m);

                    offset = IndexOf(m + factor * 2) - IndexOf(m);

                    // Don't advance to a new word unless the next
                    // bit is in the same word!
                    if (offset != 0)
                    {
                        offsets[iUsed] = offset;
                        iUsed++;
                        cumOffset += offset;
                    }
                }

                // In this case, the bits in the last word can just
                // be merged to the first.
                if (offset == 0)
                {
                    masks[0] |= masks[iUsed];
                }

                // In all cases, iUsed will be 1 BEYOND the last mask used.

                // Now just rip through the array or-ing in these masks in an
                // identical pattern.
                int iStop = IndexOf(SieveSize);
                int i = baseVal;

                for (; i <= iStop - cumOffset;)
                {
                    for (int j = 0; j < iUsed; j++)
                    {
                        data[i] |= masks[j];
                        i += offsets[j];
                    }
                }

                // Finish last few words being careful about array bounds.
                for (int j = 0; j < iUsed && i <= iStop; j++)
                {
                    data[i] |= masks[j];
                    i += offsets[j];
                }


                ClearBit(factor);
            }
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            yield return 2;
            yield return 3;
            yield return 5;

            for (int num = 7, step = 1, inc = steps[step];
                 num <= SieveSize;
                 num += inc, step = (step + 1) % 8, inc = steps[step])
            {
                if (GetBit(num) == false)
                {
                    yield return num;
                }
            }
        }


        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static int IndexOf(int n) => n >> IndexScale;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static ulong MaskOf(int n) => 1UL << ((n >> MaskScale) & dataBitsMask);

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private ref ulong GetRawBits(int index)
        {
            return ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(data), index);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private bool GetBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1);
            //int shift = ((number >> 1) & dataBitsMask);
            //ulong mask = 1ul << shift;

            //return (data[index] & mask) != 0;

            return (GetRawBits(number >> (dataBitsShift + 1)) & (1ul << ((number >> 1) & dataBitsMask))) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1);
            //int shift = ((number >> 1) & dataBitsMask);
            //ulong mask = ~(1ul << shift);

            //data[index] &= mask;

            GetRawBits(number >> (dataBitsShift + 1)) &= ~(1ul << ((number >> 1) & dataBitsMask));
        }
    }
}
