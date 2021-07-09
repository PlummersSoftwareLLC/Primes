using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp
{
    class PrimeSieveUnMod : ISieve
    {
        public string QuickName => "bench+uint+mod";
        public string Name => "Benchmark BitArray Unsigned 'mod'";
        public int? BitsPerPrime => 1;
        public int SieveSize { get; }
        private readonly BitArray bitArray;

        public PrimeSieveUnMod(int sieveSize)
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

            for (uint num = 3; num <= SieveSize; num += 2)
            {
                if (GetBit(num))
                {
                    yield return (int)num;
                }
            }
        }

        public void Run()
        {
            uint factor = 3;
            uint q = (uint)Math.Sqrt(SieveSize);

            while (factor <= q)
            {
                for (uint num = factor; num <= q; num += 2)
                {
                    if (GetBit(num))
                    {
                        factor = num;
                        break;
                    }
                }

                uint increment = factor * 2;

                for (uint num = factor * factor; num <= SieveSize; num += increment)
                    ClearBit(num);

                factor += 2;
            }
        }

        private bool GetBit(uint index)
        {
            if (index % 2 == 0)
                return false;

            return bitArray[(int)(index >> 1)];
        }

        private void ClearBit(uint index)
        {
            if (index % 2 == 1)
            {
                bitArray[(int)(index >> 1)] = false;
            }
        }
    }
}
