using System;
using System.Collections;
using System.Collections.Generic;

namespace PrimeCSharp.V2Sieves
{
    public class Bool30Sieve : ISieveRunner
    {
        public string Name => "bool30";
        public string Description => "Bool array, 8 of 30";
        public string AlgorithmType => "wheel";
        public int SieveSize { get; }
        public int ClearCount { get; set; }
        public int BitsPerPrime => 8;

        private readonly bool[] boolArray;

        public Bool30Sieve(int sieveSize)
        {
            SieveSize = sieveSize;
            
            boolArray = new bool[(sieveSize + 1) / 2];
            boolArray.AsSpan().Fill(true);
        }

        const int stepsLength = 8;

        // Only numbers congruent to candidates mod 30 can be prime.
        // Only for informative purposes
        //int[] candidates = new int[stepsLength] { 1, 7, 11, 13, 17, 19, 23, 29 };

        // Steps are the distances to the next candidate
        readonly int[] steps = new int[stepsLength] { 6, 4, 2, 4, 2, 4, 6, 2 };

        public void Run()
        {
            int start = 7;
            int q = (int)Math.Sqrt(SieveSize);

            // We can skip 2, 3, and 5, and start our checks with 7.
            // This will be index 1 in the steps list.

            int step = 1;

            for (int factor = start, inc = steps[step];
                factor <= q;
                factor += inc, step = (step + 1) & 7, inc = steps[step])
            {
                if (GetBit(factor))
                {
                    for (int num = factor * factor, iStep = step, nInc = steps[iStep];
                        num <= SieveSize;
                        num += factor * nInc, iStep = (iStep + 1) & 7, nInc = steps[iStep]
                        )
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
            yield return 5;

            for (int num = 7, step = 1, inc = steps[step];
                 num <= SieveSize;
                 num += inc, step = (step + 1) % 8, inc = steps[step])
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
