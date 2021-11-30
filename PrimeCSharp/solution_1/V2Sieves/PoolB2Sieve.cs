using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.V2Sieves
{
    public class PoolB2Sieve : ISieveRunner
    {
        public string Name => "poolb2";
        public string Description => "Array pool, 8 bit, 1 of 2";
        public string AlgorithmType => "base";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly byte[] data;
        private const int dataBits = 8;
        private const int dataBitsShift = 3;
        private const int dataBitsMask = 7;

        public PoolB2Sieve(int sieveSize)
        {
            SieveSize = sieveSize;

            data = ArrayPool<byte>.Shared.Rent((SieveSize >> (dataBitsShift + 1)) + 1);
            data.AsSpan().Fill(byte.MaxValue);
        }

        public void Run()
        {
            int q = (int)Math.Sqrt(SieveSize);

            for (int factor = 3; factor <= q; factor += 2)
            {
                if (GetBit(factor))
                {
                    int increment = factor << 1;

                    for (int num = factor * factor; num <= SieveSize; num += increment)
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
            //byte mask = (byte)(1 << shift);

            //return (data[index] & mask) != 0;

            return (data[number >> (dataBitsShift + 1)] & (byte)(1 << ((number >> 1) & dataBitsMask))) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1);
            //int shift = ((number >> 1) & dataBitsMask);
            //byte mask = (byte)~(1 << shift);

            //data[index] &= mask;

            data[number >> (dataBitsShift + 1)] &= (byte)~(1 << ((number >> 1) & dataBitsMask));
        }
    }
}
