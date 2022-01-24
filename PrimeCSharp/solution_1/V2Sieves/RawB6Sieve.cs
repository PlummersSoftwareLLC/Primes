using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace PrimeCSharp.V2Sieves
{
    public class RawB6Sieve : ISieveRunner
    {
        public string Name => "rawb6";
        public string Description => "Raw data, 8 bit, 2 of 6";
        public string AlgorithmType => "wheel";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly byte[] data;
        private const int dataBits = 8;
        private const int dataBitsShift = 3;
        private const int dataBitsMask = 7;

        public RawB6Sieve(int sieveSize)
        {
            SieveSize = sieveSize;

            data = GC.AllocateUninitializedArray<byte>((SieveSize >> (dataBitsShift + 1)) + 1, pinned: true);
            data.AsSpan().Fill(byte.MaxValue);
        }

        public void Run()
        {
            int q = (int)Math.Sqrt(SieveSize);

            for (int factor = 5, inc = 2; factor <= q; factor += inc, inc = 6 - inc)
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
        private ref byte GetRawBits(int index)
        {
            return ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(data), index);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private bool GetBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1); 
            //int shift = ((number >> 1) & dataBitsMask);
            //byte mask = (byte)(1 << shift);

            //return (data[index] & mask) != 0;

            //return (data[number >> (dataBitsShift + 1)] & (byte)(1 << ((number >> 1) & dataBitsMask))) != 0;
            return (GetRawBits(number >> (dataBitsShift + 1)) & (byte)(1 << ((number >> 1) & dataBitsMask))) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(int number)
        {
            System.Diagnostics.Debug.Assert(number % 2 == 1);

            //int index = number >> (dataBitsShift + 1);
            //int shift = ((number >> 1) & dataBitsMask);
            //byte mask = (byte)~(1 << shift);

            //data[index] &= mask;

            //data[number >> (dataBitsShift + 1)] &= (byte)~(1 << ((number >> 1) & dataBitsMask));
            GetRawBits(number >> (dataBitsShift + 1)) &= (byte)~(1 << ((number >> 1) & dataBitsMask));
        }
    }
}
