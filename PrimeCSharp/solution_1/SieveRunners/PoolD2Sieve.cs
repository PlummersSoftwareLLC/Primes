using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.SieveRunners
{
    public class PoolD2Sieve : ISieveRunner
    {
        public string Name => "PoolD2";
        public string Description => "Array pool, dword, 1 of 2";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly uint[] data;
        private const int dataBits = 32;
        private const int dataBitsShift = 5;
        private const int dataBitsMask = 31;

        public PoolD2Sieve(int sieveSize)
        {
            if (sieveSize < 0)
                throw new ArgumentOutOfRangeException(nameof(sieveSize));

            SieveSize = sieveSize;

            data = ArrayPool<uint>.Shared.Rent((SieveSize >> (dataBitsShift + 1)) + 1);
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
        private bool GetBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1);
            //int shift = ((number >> 1) & dataBitsMask);
            //uint mask = 1u << shift;

            //return (data[index] & mask) != 0;

            return (data[number >> (dataBitsShift + 1)] & (1u << ((number >> 1) & dataBitsMask))) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1);
            //int shift = ((number >> 1) & dataBitsMask);
            //uint mask = ~(1u << shift);

            //data[index] &= mask;

            data[number >> (dataBitsShift + 1)] &= ~(1u << ((number >> 1) & dataBitsMask));
        }
    }
}
