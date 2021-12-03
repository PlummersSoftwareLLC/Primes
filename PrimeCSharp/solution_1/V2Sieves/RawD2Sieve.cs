using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace PrimeCSharp.V2Sieves
{
    public class RawD2Sieve : ISieveRunner
    {
        public string Name => "rawd2";
        public string Description => "Raw data, 32 bit, 1 of 2";
        public string AlgorithmType => "base";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly uint[] data;
        private const int dataBits = 32;
        private const int dataBitsShift = 5;
        private const int dataBitsMask = 31;

        public RawD2Sieve(int sieveSize)
        {
            if (sieveSize < 0)
                throw new ArgumentOutOfRangeException(nameof(sieveSize));

            SieveSize = sieveSize;

            data = GC.AllocateUninitializedArray<uint>((SieveSize >> (dataBitsShift + 1)) + 1, pinned: true);
            data.AsSpan().Fill(uint.MaxValue);
        }

        public void Run()
        {
            int factor = 3;
            int q = (int)Math.Sqrt(SieveSize);

            for (int f = factor; f <= q; f += 2)
            {
                if (GetBit(f))
                {
                    int increment = f * 2;

                    for (int num = f * f; num <= SieveSize; num += increment)
                    {
                        ClearBit(num);
                    }
                }
            }
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            yield return 2;

            for (int num = 3; num <= SieveSize; num += 2)
            {
                if (GetBit(num))
                {
                    yield return (int)num;
                }
            }
        }


        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private ref uint GetRawBits(int index)
        {
            return ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(data), index);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private bool GetBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1);
            //int shift = ((number >> 1) & dataBitsMask);
            //uint mask = 1u << shift;

            //return (data[index] & mask) != 0;

            //return (data[number >> (dataBitsShift + 1)] & (1u << ((number >> 1) & dataBitsMask))) != 0;
            return (GetRawBits(number >> (dataBitsShift + 1)) & (1u << ((number >> 1) & dataBitsMask))) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1);
            //int shift = ((number >> 1) & dataBitsMask);
            //uint mask = ~(1u << shift);

            //data[index] &= mask;

            //data[number >> (dataBitsShift + 1)] &= ~(1u << ((number >> 1) & dataBitsMask));
            GetRawBits(number >> (dataBitsShift + 1)) &= ~(1u << ((number >> 1) & dataBitsMask));
        }
    }
}
