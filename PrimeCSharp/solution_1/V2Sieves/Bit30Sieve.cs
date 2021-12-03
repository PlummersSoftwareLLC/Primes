using System;
using System.Collections;
using System.Collections.Generic;

namespace PrimeCSharp.V2Sieves
{
    public class Bit30Sieve : ISieveRunner
    {
        public string Name => "bit30";
        public string Description => "Bitarray, 8 of 30";
        public string AlgorithmType => "wheel";
        public int SieveSize { get; }
        public int ClearCount { get; set; }

        private readonly BitArray bitArray;

        public Bit30Sieve(int sieveSize)
        {
            SieveSize = sieveSize;
            bitArray = new((sieveSize + 1) >> 1, true);
        }


        const int stepsLength = 8;

        // Only numbers congruent to candidates mod 30 can be prime.
        // Only for informative purposes
        //int[] candidates = new int[stepsLength] { 1, 7, 11, 13, 17, 19, 23, 29 };

        // Steps are the distances to the next candidate
        readonly int[] steps = new int[stepsLength] { 6, 4, 2, 4, 2, 4, 6, 2 };


        public void Run()
        {
            int q = (int)Math.Sqrt(SieveSize);

            // We can skip 2, 3, and 5, and start our checks with 7.
            // This will be index 1 in the steps list.

            int step = 1;

            for (int factor = 7, inc = steps[step];
                factor <= q;
                factor += inc, step = (step + 1) & 7, inc = steps[step]
                )
            {
                if (GetBit(factor))
                {
                    for (int num = factor * factor, iStep = step, nInc = steps[iStep];
                        num <= SieveSize;
                        )
                    {
                        ClearBit(num);

                        num += factor * nInc;

                        // This is 50% faster overall than using the %8 logic:
                        if (++iStep == 8)
                            iStep = 0;

                        nInc = steps[iStep];
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
                 num += inc, step = (step + 1) & 7, inc = steps[step])
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
