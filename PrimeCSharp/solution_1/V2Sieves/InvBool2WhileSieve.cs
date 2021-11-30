using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PrimeCSharp.V2Sieves
{
    public class InvBool2WhileSieve : ISieveRunner
    {
        public string Name => "invbool2while";
        public string Description => "Bool array, 1 of 2, invert array, while";
        public string AlgorithmType => "base";
        public int SieveSize { get; }
        public int ClearCount { get; set; }
        public int BitsPerPrime => 8;

        private readonly bool[] boolArray;

        public InvBool2WhileSieve(int sieveSize)
        {
            SieveSize = sieveSize;
            
            boolArray = new bool[(sieveSize + 1) >> 1];
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

                int increment = factor + factor;

                for (int num = factor * factor; num <= SieveSize; num += increment)
                {
                    SetBit(num);
                }

                factor += 2;
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


        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private bool GetBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            return boolArray[index >> 1];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void SetBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            boolArray[index >> 1] = true;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ClearBit(int index)
        {
            System.Diagnostics.Debug.Assert(index % 2 == 1);

            boolArray[index >> 1] = false;
        }
    }
}
