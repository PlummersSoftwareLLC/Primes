using System;
using System.Collections.Generic;

namespace PrimeCSharp.V2Sieves
{
    public class Bool2Sieve : ISieveRunner
    {
        public string Name => "bool2";
        public string Description => "Bool array, 1 of 2";
        public string AlgorithmType => "base";
        public int SieveSize { get; }
        public int ClearCount { get; set; }
        public int BitsPerPrime => 8;

        private readonly bool[] boolArray;

        public Bool2Sieve(int sieveSize)
        {
            SieveSize = sieveSize;
            
            boolArray = new bool[(sieveSize + 1) >> 1];
            boolArray.AsSpan().Fill(true);
        }

        public void Run()
        {
            int q = (int)Math.Sqrt(SieveSize);

            for (int factor = 3; factor <= q; factor += 2)
            {
                if (GetBit(factor))
                {
                    int increment = factor + factor;

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
