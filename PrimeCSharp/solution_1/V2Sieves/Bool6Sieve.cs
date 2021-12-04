using System;
using System.Collections;
using System.Collections.Generic;

namespace PrimeCSharp.V2Sieves
{
    public class Bool6Sieve : ISieveRunner
    {
        public string Name => "bool6";
        public string Description => "Bool array, 2 of 6";
        public string AlgorithmType => "wheel";
        public int SieveSize { get; }
        public int ClearCount { get; set; }
        public int BitsPerPrime => 8;

        private readonly bool[] boolArray;

        public Bool6Sieve(int sieveSize)
        {
            SieveSize = sieveSize;
            
            boolArray = new bool[(sieveSize + 1) >> 1];
            boolArray.AsSpan().Fill(true);
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


        private bool GetBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            return boolArray[index >> 1];
        }

        private void ClearBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            boolArray[index >> 1] = false;
        }
    }
}
