using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveRawBits2Of6 : ISieve
    {
        public string QuickName => "raw6";
        public string Name => "Raw Allocation, 2 of 6";
        public string AlgorithmType => "other";
        public int? BitsPerPrime => 1;

        public int SieveSize { get; }
        private readonly byte[] rawbits;
        private const int elementBits = 8;

        public PrimeSieveRawBits2Of6(int sieveSize)
        {
            if (sieveSize < 3)
            {
                throw new ArgumentOutOfRangeException(nameof(sieveSize), sieveSize,
                    $"Sieve size must be a positive integer greater than 2.");
            }

            SieveSize = sieveSize;

            rawbits = GC.AllocateUninitializedArray<byte>((sieveSize / elementBits / 2) + 1, pinned: true);
            rawbits.AsSpan().Fill(0xFF);
        }

        public int CountPrimes()
        {
            int count = 2;
            for (int i = 5, step = 2; i < SieveSize; i += step, step = 6 - step)
                if (GetBit(rawbits, i))
                    count++;
            return count;
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            List<int> result = new() { 2, 3 };

            for (int index = 5, step = 2; index <= SieveSize; index += step, step = 6 - step)
            {
                if (GetBit(rawbits, index))
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

            for (int factor = 5, inc = 2; factor <= q; factor += inc, inc = 6 - inc)
            {
                if (GetBit(rawbits, factor))
                {
                    for (int num = factor * factor, nInc = inc; num <= SieveSize; num += factor * nInc, nInc = 6 - nInc)
                    {
                        ClearBit(rawbits, num);
                        //ClearCount++;
                    }
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static bool GetBit(byte[] bits, int index)
        {
            index /= 2;

            uint mask = 1u << index % elementBits;

            return (bits[index / elementBits] & mask) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static void ClearBit(byte[] bits, int index)
        {
            index /= 2;

            byte mask = (byte)~(1u << index % elementBits);

            bits[index / elementBits] &= mask;
        }
    }
}
