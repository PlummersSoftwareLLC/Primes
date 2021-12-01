using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.V2Sieves
{
    public class PoolD6Sieve : ISieveRunner
    {
        public string Name => "poold6";
        public string Description => "Array pool, 32 bit, 2 of 6";
        public string AlgorithmType => "wheel";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly uint[] data;
        private const int dataBits = 32;
        private const int dataBitsShift = 5;
        private const int dataBitsMask = 31;

        public PoolD6Sieve(int sieveSize)
        {
            if (sieveSize < 0)
                throw new ArgumentOutOfRangeException(nameof(sieveSize));

            SieveSize = sieveSize;

            data = ArrayPool<uint>.Shared.Rent((SieveSize >> (dataBitsShift + 1)) + 1);
            data.AsSpan().Fill(uint.MaxValue);
        }

        public void Run()
        {
            int start = 5;
            int q = (int)Math.Sqrt(SieveSize);

            for (int factor = start, inc = 2; factor <= q; factor += inc, inc = 6 - inc)
            {
                if (GetBit(factor))
                {
                    for (int num = factor * factor, nInc = inc; num <= SieveSize; num += factor * nInc, nInc = 6 - nInc)
                    {
                        ClearBit(num);
                    }
                }
            }
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            yield return 2;
            yield return 3;

            for (int num = 5, inc = 2; num <= SieveSize; num += inc, inc = 6 - inc)
            {
                if (GetBit(num))
                {
                    yield return num;
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
