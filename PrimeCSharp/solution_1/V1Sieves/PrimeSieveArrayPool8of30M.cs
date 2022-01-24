using System;
using System.Buffers;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveArrayPool8of30M : ISieve
    {
        public string QuickName => "pool30m";
        public string Name => "Array Pool, 8 of 30, Bitmasking";
        public string AlgorithmType => "wheel+other";
        public int? BitsPerPrime => 1;

        public int SieveSize { get; }

        // The primes data
        private readonly ulong[] data;
        const int BitsPerWord = sizeof(ulong) * 8;
        const int PowerScale = 6; // 2^6 = 64 == BitsPerWord
        const int MaskScale = 1; // 0 for keeping all values, 1 for only odd numbers, etc
        const int IndexScale = PowerScale + MaskScale;

        // Only numbers congruent to candidates mod 30 can be prime.
        // Only for informative purposes
        //int[] candidates = new int[8] { 1, 7, 11, 13, 17, 19, 23, 29 };

        // Steps are the distances to the next candidate
        readonly int[] steps = new int[8] { 6, 4, 2, 4, 2, 4, 6, 2 };

        public PrimeSieveArrayPool8of30M(int sieveSize)
        {
            if (sieveSize < 10)
            {
                throw new ArgumentOutOfRangeException(nameof(sieveSize), sieveSize,
                    $"Sieve size must be at least 10.");
            }

            SieveSize = sieveSize;
            data = ArrayPool<ulong>.Shared.Rent((SieveSize >> IndexScale) + 1);
        }

        public int CountPrimes()
        {
            // Get 2, 3, and 5 for free
            int count = 3;

            var bits = data.AsSpan();

            for (int index = 7, step = 1, inc = steps[step];
                 index <= SieveSize;
                 index += inc, step = (step + 1) % 8, inc = steps[step])
            {
                if (GetBit(ref bits, index) == false) count++;
            }

            return count;
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            // Get 2, 3, and 5 for free
            List<int> result = new() { 2, 3, 5 };

            var bits = data.AsSpan();

            for (int index = 7, step = 1, inc = steps[step];
                 index <= SieveSize;
                 index += inc, step = (step + 1) % 8, inc = steps[step])
            {
                if (GetBit(ref bits, index) == false)
                {
                    result.Add(index);
                }
            }

            return result;
        }

        public int ClearCount { get; set; }

        public void Run()
        {
            Span<ulong> bits = data.AsSpan();
            bits.Fill(0);

            int q = (int)Math.Sqrt(SieveSize);

            // We can skip 2, 3, and 5, and start our checks with 7.
            // This will be index 1 in the steps list.

            // Cached bitmaps and index offsets for bit twiddling loop.
            ulong[] masks = new ulong[BitsPerWord];
            int[] offsets = new int[BitsPerWord];

            // Look for next prime
            for (int factor = 7, step = 1, inc = steps[step];
                factor <= q;
                factor += inc, step = (step + 1) % 8, inc = steps[step])
            {
                // A set bit means it's composite - keep searching.
                if (GetBit(ref bits, factor))
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

                for (int bitCount = 0, m = factor * factor; bitCount < BitsPerWord; bitCount++, m += factor * 2)
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
                        bits[i] |= masks[j];
                        i += offsets[j];
                    }
                }

                // Finish last few words being careful about array bounds.
                for (int j = 0; j < iUsed && i <= iStop; j++)
                {
                    bits[i] |= masks[j];
                    i += offsets[j];
                }


                ClearBit(ref bits, factor);
            }

            ArrayPool<ulong>.Shared.Return(data);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static int IndexOf(int n) => n >> IndexScale;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static ulong MaskOf(int n) => 1UL << ((n >> MaskScale) % BitsPerWord);


        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static bool GetBit(ref Span<ulong> bits, int index) => (bits[index >> IndexScale] & (1UL << (index >> MaskScale))) != 0;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static void ClearBit(ref Span<ulong> bits, int index) => bits[index >> IndexScale] &= ~(1UL << (index >> MaskScale));
    }
}
