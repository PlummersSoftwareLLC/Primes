using System;
using System.Buffers;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.V1Sieves
{
    readonly ref struct PrimeSieveArrayPoolRef
    {
        public int SieveSize { get; }

        private readonly Span<Int64> bits;
        private readonly Int64[] data;

        public PrimeSieveArrayPoolRef(int sieveSize)
        {
            if (sieveSize < 3)
            {
                throw new ArgumentOutOfRangeException(nameof(sieveSize), sieveSize,
                    $"Sieve size must be a positive integer greater than 2.");
            }

            SieveSize = sieveSize;

            data = ArrayPool<Int64>.Shared.Rent(SieveSize >> 6);
            bits = data.AsSpan();
            bits.Fill(long.MaxValue);
        }

        public Int32 CountPrimes()
        {
            Int32 count = 1;

            for (Int32 index = 3; index <= SieveSize; index++)
            {
                if (GetBit(index)) count++;
            }

            return count;
        }

        public bool? IsCountValid()
        {
            return PrimeData.IsCountCorrect(SieveSize, CountPrimes());
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private Boolean GetBit(Int32 index)
        {
            return index % 2 != 0 && (bits[index >> 6] & (1 << (index >> 1))) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(Int32 index)
        {
            bits[index >> 6] &= ~(1 << (index >> 1));
        }

        public void Run()
        {
            RunSieve();
            Dispose();
        }

        public void RunSieve()
        {
            int factor = 3;
            int q = (int)Math.Sqrt(SieveSize);

            while (factor <= q)
            {
                for (int num = factor; num <= SieveSize; num++)
                {
                    if (GetBit(num))
                    {
                        factor = num;
                        break;
                    }
                }

                int factorTimes2 = factor << 1;

                for (int num = factor * factor; num <= SieveSize; num += factorTimes2)
                {
                    ClearBit(num);
                }

                factor += 2;
            }
        }

        public void Dispose()
        {
            ArrayPool<Int64>.Shared.Return(data);
        }
    }
}
