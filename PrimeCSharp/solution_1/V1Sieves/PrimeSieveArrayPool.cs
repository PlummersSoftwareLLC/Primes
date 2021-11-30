using System;
using System.Buffers;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveArrayPool : ISieve
    {
        public string QuickName => "pool";
        public string Name => "Array Pool";
        public string AlgorithmType => "base";
        public int? BitsPerPrime => 1;

        private readonly Int64[] data;

        public PrimeSieveArrayPool(int sieveSize)
        {
            SieveSize = sieveSize;
            data = ArrayPool<Int64>.Shared.Rent(SieveSize >> 6);
        }

        public int SieveSize { get; }

        public int CountPrimes()
        {
            int count = 1;

            var bits = data.AsSpan();

            for (int index = 3; index <= SieveSize; index++)
            {
                if (GetBit(ref bits, index)) count++;
            }

            return count;
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            List<int> result = new() { 2 };

            var bits = data.AsSpan();

            for (int num = 3; num <= SieveSize; num += 2)
            {
                if (GetBit(ref bits, num))
                {
                    result.Add(num);
                }
            }

            return result;
        }

        public void Run()
        {
            int factor = 3;
            int q = (int)Math.Sqrt(SieveSize);

            Span<Int64> bits = data.AsSpan();
            bits.Fill(long.MaxValue);

            while (factor <= q)
            {
                for (int num = factor; num <= SieveSize; num++)
                {
                    if (GetBit(ref bits, num))
                    {
                        factor = num;
                        break;
                    }
                }

                int factorTimes2 = factor << 1;

                for (int num = factor * factor; num <= SieveSize; num += factorTimes2)
                    ClearBit(ref bits, num);

                factor += 2;
            }

            ArrayPool<Int64>.Shared.Return(data);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static Boolean GetBit(ref Span<Int64> bits, Int32 index) => index % 2 != 0 && (bits[index >> 6] & (1 << (index >> 1))) != 0;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static void ClearBit(ref Span<Int64> bits, Int32 index) => bits[index >> 6] &= ~(1 << (index >> 1));
    }
}
