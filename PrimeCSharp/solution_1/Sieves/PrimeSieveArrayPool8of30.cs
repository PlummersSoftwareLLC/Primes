﻿using System;
using System.Buffers;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.Sieves
{
    class PrimeSieveArrayPool8of30 : ISieve
    {
        public int SieveSize { get; }

        // The primes data
        private readonly long[] data;

        const int stepsLength = 8;

        // Only numbers congruent to candidates mod 30 can be prime.
        // Only for informative purposes
        //int[] candidates = new int[stepsLength] { 1, 7, 11, 13, 17, 19, 23, 29 };

        // Steps are the distances to the next candidate
        readonly int[] steps = new int[stepsLength] { 6, 4, 2, 4, 2, 4, 6, 2 };

        public PrimeSieveArrayPool8of30(int sieveSize)
        {
            SieveSize = sieveSize;
            data = ArrayPool<long>.Shared.Rent(SieveSize >> 6);
        }

        public int CountPrimes()
        {
            int count = 3;

            var bits = data.AsSpan();

            for (int index = 7, step = 1, inc = steps[step];
                 index <= SieveSize;
                 index += inc, step = (step + 1) % 8, inc = steps[step])
            {
                if (GetBit(ref bits, index)) count++;
            }

            return count;
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            List<int> result = new() { 2, 3, 5 };

            var bits = data.AsSpan();

            for (int index = 7, step = 1, inc = steps[step];
                 index <= SieveSize;
                 index += inc, step = (step + 1) % 8, inc = steps[step])
            {
                if (GetBit(ref bits, index))
                {
                    result.Add(index);
                }
            }

            return result;
        }

        public int ClearCount { get; set; }

        public void Run()
        {
            int q = (int)Math.Sqrt(SieveSize);
            Span<long> bits = data.AsSpan();
            bits.Fill(long.MaxValue);

            // We can skip 2, 3, and 5, and start our checks with 7.
            // This will be index 1 in the steps list.

            int step = 1;

            for (int factor = 7, inc = steps[step];
                factor <= q;
                factor += inc, step = (step + 1) % 8, inc = steps[step])
            {
                if (GetBit(ref bits, factor))
                {
                    int iStep = step;

                    // This simplified version of the for loop is 5% slower:
                    //for (int num = factor * factor;
                    //    num <= SieveSize;
                    //    num += factor * steps[iStep], iStep = (iStep + 1) % 8)

                    for (int num = factor * factor, nInc = steps[iStep];
                        num <= SieveSize;
                        num += factor * nInc, iStep = (iStep + 1) % stepsLength, nInc = steps[iStep])
                    {
                        ClearBit(ref bits, num);
                        //ClearCount++;
                    }
                }
            }

            ArrayPool<long>.Shared.Return(data);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static bool GetBit(ref Span<long> bits, int index) => (bits[index >> 6] & (1 << (index >> 1))) != 0;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static void ClearBit(ref Span<long> bits, int index) => bits[index >> 6] &= ~(1 << (index >> 1));
    }
}
