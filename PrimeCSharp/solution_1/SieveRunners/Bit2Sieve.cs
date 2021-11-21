using System;
using System.Collections;
using System.Collections.Generic;

namespace PrimeCSharp.SieveRunners
{
    public class Bit2Sieve : ISieveRunner
    {
        public string Name => "Bit2";
        public string Description => "Bitarray, 1 of 2";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly BitArray bitArray;

        public Bit2Sieve(int sieveSize)
        {
            SieveSize = sieveSize;
            bitArray = new((sieveSize + 1) / 2, true);
        }

        public void Run()
        {
            int factor = 3;
            int q = (int)Math.Sqrt(SieveSize);

            for (int f = factor; f <= q; f += 2)
            {
                if (GetBit(f))
                {
                    int increment = f * 2;

                    for (int num = f * f; num <= SieveSize; num += increment)
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
