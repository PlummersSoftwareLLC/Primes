using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveRawBitsInter : ISieve
    {
        public string QuickName => "raw32";
        public string Name => "Raw Allocation, uint";
        public string AlgorithmType => "base";
        public int? BitsPerPrime => 1;

        public int SieveSize { get; }
        private readonly uint[] rawbits;
        private const int elementBits = 32;

        public PrimeSieveRawBitsInter(int sieveSize)
        {
            if (sieveSize < 3)
            {
                throw new ArgumentOutOfRangeException(nameof(sieveSize), sieveSize,
                    $"Sieve size must be a positive integer greater than 2.");
            }

            SieveSize = sieveSize;

            rawbits = GC.AllocateUninitializedArray<uint>((sieveSize / elementBits / 2) + 1, pinned: true);
            rawbits.AsSpan().Fill(0xFFFFFFFF);
        }

        public int CountPrimes()
        {
            // hoist null check
            _ = GetRawBits(rawbits, 0);

            int count = 1;
            for (uint i = 3; i < SieveSize; i += 2)
                if (GetBit(rawbits, i))
                    count++;
            return count;
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            yield return 2;

            for (uint num = 3; num <= SieveSize; num += 2)
            {
                if (GetBit(rawbits, num))
                {
                    yield return (int)num;
                }
            }
        }

        public void Run()
        {
            uint factor = 3;
            uint q = (uint)Math.Sqrt(SieveSize);

            while (factor <= q)
            {
                for (uint num = factor; num <= q; num += 2)
                {
                    if (GetBit(rawbits, num))
                    {
                        factor = num;
                        break;
                    }
                }

                uint increment = factor * 2;

                for (uint num = factor * factor; num <= SieveSize; num += increment)
                    ClearBit(rawbits, num);

                factor += 2;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private ref uint GetRawBits(uint[] bits, uint index)
        {
            //return ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(rawbits), (nint)index);
            return ref bits[index];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private bool GetBit(uint[] bits, uint index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            index /= 2;

            uint raw = GetRawBits(bits, index / elementBits);
            uint mask = 1u << (int)(index % elementBits);

            return (raw & mask) != 0;
            //return (GetRawBits(index / elementBits) & (1u << (int)(index % elementBits))) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(uint[] bits, uint index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            index /= 2;

            uint mask = ~(1u << (int)(index % elementBits));

            GetRawBits(bits, index / elementBits) &= mask;

            // Test Interlocked.CompareExchange penalty
            //uint raw = GetRawBits(bits, index / elementBits);
            //uint nRaw = raw & mask;

            //while (Interlocked.CompareExchange(ref GetRawBits(bits, index / elementBits), nRaw, raw) != raw)
            //{
            //    raw = GetRawBits(bits, index / elementBits);
            //}
        }
    }
}
