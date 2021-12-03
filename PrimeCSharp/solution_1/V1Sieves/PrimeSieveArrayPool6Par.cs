using System;
using System.Buffers;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveArrayPool6Par : ISieve
    {
        public string QuickName => "pool6p";
        public string Name => "Array Pool, 2 of 6, parallel";
        public string AlgorithmType => "other";
        public int? BitsPerPrime => 1;

        private readonly long[] data;
        private readonly int threadCount;
        public bool IsParallel => true;

        public PrimeSieveArrayPool6Par(int sieveSize, int threadCount)
        {
            SieveSize = sieveSize;
            this.threadCount = threadCount > 0 ? threadCount : Environment.ProcessorCount;
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
            int blockSize = ((SieveSize - q) / threadCount) + 1;

            Span<long> bits = data.AsSpan();
            bits.Fill(long.MaxValue);

            // Figure out the primes up to sqrt(size). Will use these as a base.
            for (int factor = 5, inc = 2; factor <= q; factor += inc, inc = 6 - inc)
            {
                if (GetBit(ref bits, factor))
                {
                    for (int num = factor * factor, nInc = inc; num <= q; num += factor * nInc, nInc = 6 - nInc)
                    {
                        ClearBit(ref bits, num);
                    }
                }
            }

            for (int factor = 5, inc = 2; factor <= q; factor += inc, inc = 6 - inc)
            {
                if (GetBit(ref bits, factor))
                {
                    Parallel.For(0, threadCount, (block) =>
                    {
                        int rangeStart = q + block * blockSize;
                        int rangeEnd = Math.Min(rangeStart + blockSize, SieveSize);
                        Span<long> innerBits = data.AsSpan();

                        int mult = Math.Max(((rangeStart / factor) + 1) | 1, factor);
                        int step = 2;

                        int mod = mult % 3;
                        if (mod == 0)
                            mult += 2;
                        if (mod == 1)
                            step = 4;

                        int factorStart = mult * factor;

                        for (int num = factorStart; num <= rangeEnd; num += factor * step, step = 6 - step)
                        {
                            ClearBit(ref innerBits, num);
                        }
                    });
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
