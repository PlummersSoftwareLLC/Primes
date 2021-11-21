using System;
using System.Collections;
using System.Collections.Generic;

namespace PrimeCSharp.SieveRunners
{
    public class Bit6Sieve : ISieveRunner
    {
        public string Name => "Bit6";
        public string Description => "Bitarray, 2 of 6";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly BitArray bitArray;

        public Bit6Sieve(int sieveSize)
        {
            SieveSize = sieveSize;
            bitArray = new((sieveSize + 1) / 2, true);
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
                if (bitArray[num / 2])
                {
                    yield return num;
                }
            }
        }


        private bool GetBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            return bitArray[index / 2];
        }

        private void ClearBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            bitArray[index / 2] = false;
        }
    }
}
