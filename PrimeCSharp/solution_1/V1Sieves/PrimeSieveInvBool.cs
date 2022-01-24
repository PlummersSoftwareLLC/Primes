using System;
using System.Collections.Generic;

namespace PrimeCSharp.V1Sieves
{
    class PrimeSieveInvBool : ISieve
    {
        public string QuickName => "ibool";
        public string Name => "Inverted Bool Array";
        public string AlgorithmType => "base";
        public int? BitsPerPrime => 8;

        public int SieveSize { get; }
        private readonly bool[] boolArray;

        public PrimeSieveInvBool(int sieveSize)
        {
            if (sieveSize < 3)
            {
                throw new ArgumentOutOfRangeException(nameof(sieveSize), sieveSize,
                    $"Sieve size must be a positive integer greater than 2.");
            }

            SieveSize = sieveSize;
            boolArray = new bool[(sieveSize + 1) / 2];
        }

        public int CountPrimes()
        {
            int count = 0;
            for (int i = 0; i < boolArray.Length; i++)
                if (boolArray[i] == false)
                    count++;
            return count;
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

        public void Run()
        {
            int factor = 3;
            int q = (int)Math.Sqrt(SieveSize);

            while (factor <= q)
            {
                for (int num = factor; num <= q; num += 2)
                {
                    if (GetBit(num) == false)
                    {
                        factor = num;
                        break;
                    }
                }

                int increment = factor * 2;

                for (int num = factor * factor; num <= SieveSize; num += increment)
                    SetBit(num);

                factor += 2;
            }
        }

        private bool GetBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            return boolArray[index >> 1];
        }

        private void SetBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            boolArray[index >> 1] = true;
        }

        private void ClearBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            boolArray[index >> 1] = false;
        }
    }
}
