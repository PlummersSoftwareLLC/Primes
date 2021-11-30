using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveRawParallel : ISieve
    {
        public string QuickName => "rawp";
        public string Name => "Raw Allocation, Parallel";
        public string AlgorithmType => "base";
        public int? BitsPerPrime => 1;

        public int SieveSize { get; }
        private readonly uint[] rawbits;
        private readonly int threadCount;
        private const int elementBits = 32;
        public bool IsParallel => true;

        public PrimeSieveRawParallel(int sieveSize, int threadCount)
        {
            if (sieveSize < 3)
            {
                throw new ArgumentOutOfRangeException(nameof(sieveSize), sieveSize,
                    $"Sieve size must be a positive integer greater than 2.");
            }

            SieveSize = sieveSize;
            this.threadCount = threadCount > 0 ? threadCount : Environment.ProcessorCount;
            rawbits = GC.AllocateUninitializedArray<uint>((sieveSize / elementBits / 2) + 1, pinned: true);
            rawbits.AsSpan().Fill(0xFFFFFFFF);
        }

        public int CountPrimes()
        {
            int count = 1;
            for (int i = 3; i < SieveSize; i+=2)
                if (GetBit(rawbits, i))
                    count++;
            return count;
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            yield return 2;

            for (int num = 3; num <= SieveSize; num += 2)
            {
                if (GetBit(rawbits, num))
                {
                    yield return (int)num;
                }
            }
        }

        public void Run()
        {
            int q = (int)Math.Sqrt(SieveSize);
            int blockSize = ((SieveSize - q) / threadCount) + 1;

            // Figure out the primes up to sqrt(size). Will use these as a base.
            for (int factor = 3; factor <= q; factor += 2)
            {
                if (GetBit(rawbits, factor))
                {
                    for (int num = factor * factor; num <= q; num += factor * 2)
                    {
                        ClearBit(rawbits, num);
                    }
                }
            }

            for (int factor = 3; factor <= q; factor += 2)
            {
                if (GetBit(rawbits, factor))
                {
                    Parallel.For(0, threadCount, (block) =>
                    {
                        int rangeStart = q + block * blockSize;
                        int rangeEnd = Math.Min(rangeStart + blockSize, SieveSize);

                        int mult = Math.Max(((rangeStart / factor) + 1) | 1, factor);

                        int factorStart = mult * factor;

                        for (int num = factorStart; num <= rangeEnd; num += factor * 2)
                        {
                            ClearBit(rawbits, num);
                        }
                    });
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static ref uint GetRawBits(uint[] bits, uint index)
        {
            //return ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(rawbits), (nint)index);
            return ref bits[index];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static bool GetBit(uint[] bits, int index)
        {
            index /= 2;

            uint mask = 1u << index % elementBits;

            return (bits[index / elementBits] & mask) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static void ClearBit(uint[] bits, int index)
        {
            index /= 2;

            uint mask = ~(1u << index % elementBits);

            bits[index / elementBits] &= mask;
        }
    }
}
