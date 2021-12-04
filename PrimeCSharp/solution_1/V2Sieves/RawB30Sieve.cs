using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace PrimeCSharp.V2Sieves
{
    public class RawB30Sieve : ISieveRunner
    {
        public string Name => "rawb30";
        public string Description => "Raw data, 8 bit, 8 of 30";
        public string AlgorithmType => "wheel";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly byte[] data;
        private const int dataBits = 8;
        private const int dataBitsShift = 3;
        private const int dataBitsMask = 7;

        public RawB30Sieve(int sieveSize)
        {
            SieveSize = sieveSize;

            data = GC.AllocateUninitializedArray<byte>((SieveSize >> (dataBitsShift + 1)) + 1, pinned: true);
            data.AsSpan().Fill(byte.MaxValue);
        }

        const int stepsLength = 8;

        // Only numbers congruent to candidates mod 30 can be prime.
        // Only for informative purposes
        //int[] candidates = new int[stepsLength] { 1, 7, 11, 13, 17, 19, 23, 29 };

        // Steps are the distances to the next candidate
        readonly int[] steps = new int[stepsLength] { 6, 4, 2, 4, 2, 4, 6, 2 };

        public void Run()
        {
            int start = 7;
            int q = (int)Math.Sqrt(SieveSize);

            // We can skip 2, 3, and 5, and start our checks with 7.
            // This will be index 1 in the steps list.

            int step = 1;

            for (int factor = start, inc = steps[step];
                factor <= q;
                factor += inc, step = (step + 1) % 8, inc = steps[step])
            {
                if (GetBit(factor))
                {
                    for (int num = factor * factor, iStep = step, nInc = steps[iStep];
                        num <= SieveSize;)
                    {
                        ClearBit(num);

                        num += factor * nInc;

                        // This is 50% faster overall than using the %8 logic:
                        if (++iStep == 8)
                            iStep = 0;

                        nInc = steps[iStep];
                    }
                }
            }
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            yield return 2;
            yield return 3;
            yield return 5;

            for (int num = 7, step = 1, inc = steps[step];
                 num <= SieveSize;
                 num += inc, step = (step + 1) % 8, inc = steps[step])
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
