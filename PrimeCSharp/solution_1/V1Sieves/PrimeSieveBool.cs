using System;
using System.Collections.Generic;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveBool : ISieve
    {
        public string QuickName => "bool";
        public string Name => "Bool Array";
        public string AlgorithmType => "base";
        public int? BitsPerPrime => 8;

        public int SieveSize { get; }
        private readonly bool[] boolArray;

        public PrimeSieveBool(int sieveSize)
        {
            if (sieveSize < 3)
            {
                throw new ArgumentOutOfRangeException(nameof(sieveSize), sieveSize,
                    $"Sieve size must be a positive integer greater than 2.");
            }

            SieveSize = sieveSize;
            boolArray = new bool[(sieveSize + 1) / 2];

            for (int i = 0; i < boolArray.Length; i++)
                boolArray[i] = true;
        }

        public int CountPrimes()
        {
            int count = 0;
            for (int i = 0; i < boolArray.Length; i++)
                if (boolArray[i])
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

                for (int num = factor * factor; num <= SieveSize; num += increment)
                    ClearBit(num);

                factor += 2;
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
