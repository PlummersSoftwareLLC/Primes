using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.V2Sieves
{
    public class InvBool2Sieve : ISieveRunner
    {
        public string Name => "invbool2";
        public string Description => "Bool array, 1 of 2, invert array";
        public string AlgorithmType => "base";
        public int SieveSize { get; }
        public int ClearCount { get; set; }
        public int BitsPerPrime => 8;

        private readonly bool[] boolArray;

        public InvBool2Sieve(int sieveSize)
        {
            SieveSize = sieveSize;
            
            boolArray = new bool[(sieveSize + 1) >> 1];
        }

        public void Run()
        {
            int q = (int)Math.Sqrt(SieveSize);

            for (int factor = 3; factor <= q; factor += 2)
            {
                if (GetBit(factor) == false)
                {
                    int increment = factor + factor;

                    for (int num = factor * factor; num <= SieveSize; num += increment)
                    {
                        SetBit(num);
                    }
                }
            }
        }

        public IEnumerable<int> GetFoundPrimes()
        {
            yield return 2;

            for (int num = 3; num <= SieveSize; num += 2)
            {
                if (GetBit(num) == false)
                {
                    yield return num;
                }
            }
        }


        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private bool GetBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            return boolArray[index >> 1];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void SetBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            boolArray[index >> 1] = true;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            boolArray[index >> 1] = false;
        }
    }
}
