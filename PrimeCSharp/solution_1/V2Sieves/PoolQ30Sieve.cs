using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.V2Sieves
{
    public class PoolQ30Sieve : ISieveRunner
    {
        public string Name => "poolq30";
        public string Description => "Array pool, 64 bit, 8 of 30";
        public string AlgorithmType => "wheel";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly ulong[] data;
        private const int dataBits = 64;
        private const int dataBitsShift = 6;
        private const int dataBitsMask = 63;

        public PoolQ30Sieve(int sieveSize)
        {
            if (sieveSize < 0)
                throw new ArgumentOutOfRangeException(nameof(sieveSize));

            SieveSize = sieveSize;

            data = ArrayPool<ulong>.Shared.Rent((SieveSize >> (dataBitsShift + 1)) + 1);
            data.AsSpan().Fill(ulong.MaxValue);
        }

        const int stepsLength = 8;

        // Only numbers congruent to candidates mod 30 can be prime.
        // Only for informative purposes
        //int[] candidates = new int[stepsLength] { 1, 7, 11, 13, 17, 19, 23, 29 };

        // Steps are the distances to the next candidate
        readonly int[] steps = new int[stepsLength] { 6, 4, 2, 4, 2, 4, 6, 2 };

        public void Run()
        {
            int start = 7;
            int q = (int)Math.Sqrt(SieveSize);

            // We can skip 2, 3, and 5, and start our checks with 7.
            // This will be index 1 in the steps list.

            int step = 1;

            for (int factor = start, inc = steps[step];
                factor <= q;
                factor += inc, step = (step + 1) & 7, inc = steps[step])
            {
                if (GetBit(factor))
                {
                    for (int num = factor * factor, iStep = step, nInc = steps[iStep];
                        num <= SieveSize;
                        )
                    {
                        ClearBit(num);

                        num += factor * nInc;

                        // This is 50% faster overall than using the %8 logic:
                        if (++iStep == 8)
                            iStep = 0;

                        nInc = steps[iStep];
                    }
                }
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
                if (GetBit(num))
                {
                    yield return num;
                }
            }
        }


        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private bool GetBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1);
            //int shift = ((number >> 1) & dataBitsMask);
            //ulong mask = 1ul << shift;

            //return (data[index] & mask) != 0;

            return (data[number >> (dataBitsShift + 1)] & (1ul << ((number >> 1) & dataBitsMask))) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1);
            //int shift = ((number >> 1) & dataBitsMask);
            //ulong mask = ~(1ul << shift);

            //data[index] &= mask;

            data[number >> (dataBitsShift + 1)] &= ~(1ul << ((number >> 1) & dataBitsMask));
        }
    }
}
