using System;
using System.Collections;
using System.Collections.Generic;

namespace PrimeCSharp.V2Sieves
{
    public class Bit2Sieve : ISieveRunner
    {
        public string Name => "bit2";
        public string Description => "Bitarray, 1 of 2";
        public string AlgorithmType => "base";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly BitArray bitArray;

        public Bit2Sieve(int sieveSize)
        {
            SieveSize = sieveSize;
            bitArray = new((sieveSize + 1) >> 1, true);
        }

        public void Run()
        {
            int q = (int)Math.Sqrt(SieveSize);

            for (int factor = 3; factor <= q; factor += 2)
            {
                if (GetBit(factor))
                {
                    for (int num = factor * factor; num <= SieveSize; num += factor << 1)
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

            return bitArray[index >> 1];
        }

        private void ClearBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            bitArray[index >> 1] = false;
        }
    }
}
