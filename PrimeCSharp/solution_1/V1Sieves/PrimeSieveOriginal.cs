using System;
using System.Collections;
using System.Collections.Generic;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveOriginal : ISieve
    {
        public string QuickName => "original";
        public string Name => "Original";
        public string AlgorithmType => "base";
        public int? BitsPerPrime => 1;
        public int SieveSize { get; }
        private readonly BitArray bitArray;

        public PrimeSieveOriginal(int sieveSize)
        {
            if (sieveSize < 3)
            {
                throw new ArgumentOutOfRangeException(nameof(sieveSize), sieveSize,
                    $"Sieve size must be a positive integer greater than 2.");
            }

            SieveSize = sieveSize;
            bitArray = new((sieveSize + 1) / 2, true);
        }

        public int CountPrimes()
        {
            int count = 0;
            for (int i = 0; i < bitArray.Count; i++)
                if (bitArray[i])
                    count++;
            return count;
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

        public void Run()
        {
            int factor = 3;
            int q = (int)Math.Sqrt(SieveSize);

            while (factor <= q)
            {
                for (int num = factor; num <= q; num += 2)
                {
                    if (GetBit(num))
                    {
                        factor = num;
                        break;
                    }
                }

                int increment = factor * 2;

                for (int num = factor * 3; num <= SieveSize; num += increment)
                    ClearBit(num);

                factor += 2;
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

            bitArray[index / 2] = false;
        }
    }
}
