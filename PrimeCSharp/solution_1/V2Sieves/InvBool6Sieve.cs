using System;
using System.Collections;
using System.Collections.Generic;

namespace PrimeCSharp.V2Sieves
{
    public class InvBool6Sieve : ISieveRunner
    {
        public string Name => "invbool6";
        public string Description => "Bool array, 2 of 6, invert array";
        public string AlgorithmType => "wheel";
        public int SieveSize { get; }
        public int ClearCount { get; set; }
        public int BitsPerPrime => 8;

        private readonly bool[] boolArray;

        public InvBool6Sieve(int sieveSize)
        {
            SieveSize = sieveSize;
            
            boolArray = new bool[(sieveSize + 1) / 2];
        }

        public void Run()
        {
            int start = 5;
            int q = (int)Math.Sqrt(SieveSize);

            for (int factor = start, inc = 2; factor <= q; factor += inc, inc = 6 - inc)
            {
                if (GetBit(factor) == false)
                {
                    for (int num = factor * factor, nInc = inc; num <= SieveSize; num += factor * nInc, nInc = 6 - nInc)
                    {
                        SetBit(num);
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
                if (GetBit(num) == false)
                {
                    yield return num;
                }
            }
        }


        private bool GetBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            return boolArray[index / 2];
        }

        private void SetBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            boolArray[index / 2] = true;
        }

        private void ClearBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            boolArray[index / 2] = false;
        }
    }
}
