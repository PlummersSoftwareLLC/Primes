using System;
using System.Collections;
using System.Collections.Generic;

namespace PrimeCSharp.SieveRunners
{
    public class IBool2Sieve : ISieveRunner
    {
        public string Name => "IBool2";
        public string Description => "Bool array, 1 of 2, invert array";
        public int SieveSize { get; }
        public int ClearCount { get; set; }
        public int BitsPerPrime => 8;

        private readonly bool[] boolArray;

        public IBool2Sieve(int sieveSize)
        {
            SieveSize = sieveSize;
            
            boolArray = new bool[(sieveSize + 1) / 2];
        }

        public void Run()
        {
            int factor = 3;
            int q = (int)Math.Sqrt(SieveSize);

            for (int f = factor; f <= q; f += 2)
            {
                if (GetBit(f) == false)
                {
                    int increment = f * 2;

                    for (int num = f * f; num <= SieveSize; num += increment)
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
