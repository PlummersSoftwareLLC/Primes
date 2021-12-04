using System;
using System.Buffers;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveArrayPool2Of6 : ISieve
    {
        public string QuickName => "pool2of6";
        public string Name => "Array Pool, 2 of 6";
        public string AlgorithmType => "other";
        public int? BitsPerPrime => 1;

        private readonly long[] data;

        public PrimeSieveArrayPool2Of6(int sieveSize)
        {
            SieveSize = sieveSize;
            data = ArrayPool<long>.Shared.Rent(SieveSize >> 6);
        }

        public int SieveSize { get; }

        public int CountPrimes()
        {
            int count = 2;

            var bits = data.AsSpan();

            for (int index = 5, step = 2; index <= SieveSize; index += step, step = 6 - step)
            {
                if (GetBit(ref bits, index)) count++;
            }

            return count;
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            List<int> result = new() { 2, 3 };

            var bits = data.AsSpan();

            for (int index = 5, step = 2; index <= SieveSize; index += step, step = 6 - step)
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

            for (int factor = 5, inc = 2; factor <= q; factor += inc, inc = 6 - inc)
            {
                if (GetBit(ref bits, factor))
                {
                    for (int num = factor * factor, nInc = inc; num <= SieveSize; num += factor * nInc, nInc = 6 - nInc)
                    {
                        ClearBit(ref bits, num);
                        //ClearCount++
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
