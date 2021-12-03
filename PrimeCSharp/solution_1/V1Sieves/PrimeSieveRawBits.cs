using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveRawBits : ISieve
    {
        public string QuickName => "raw";
        public string Name => "Raw Allocation";
        public string AlgorithmType => "base";
        public int? BitsPerPrime => 1;

        public int SieveSize { get; }
        private readonly byte[] rawbits;
        private const int elementBits = 8;

        public PrimeSieveRawBits(int sieveSize)
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

        public int ClearCount { get; set; }

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
                {
                    ClearBit(rawbits, num);
                }

                factor += 2;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private ref byte GetRawBits(byte[] bits, uint index)
        {
            //return ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(rawbits), (nint)index);
            return ref bits[index];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private bool GetBit(byte[] bits, uint index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            index /= 2;

            byte rawByte = GetRawBits(bits, index / elementBits);
            uint mask = 1u << (int)(index % elementBits);

            return (rawByte & mask) != 0;
            //return (GetRawBits(index / elementBits) & (1u << (int)(index % elementBits))) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(byte[] bits, uint index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);
            //ClearCount++;

            index /= 2;

            byte mask = (byte)~(1u << (int)(index % elementBits));

            GetRawBits(bits, index / elementBits) &= mask;
        }
    }
}
