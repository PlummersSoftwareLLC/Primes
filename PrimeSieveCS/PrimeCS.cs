// ---------------------------------------------------------------------------
// PrimeCS.cs : Dave's Garage Prime Sieve in C#
// ---------------------------------------------------------------------------

using System;
using System.Collections;
using System.Collections.Generic;

namespace PrimeSieveCS
{
    class PrimeCS
    {
        #region Test parameters
        const long SieveSize = (long)1e6;
#if DEBUG
        const double Duration = 1;
#else
        const double Duration = 5;
#endif
        #endregion

        class PrimeSieve {
            /// <summary> Contains historical data for validation purposes. </summary>
            private static readonly Dictionary<long, int> historicalData = new () 
            { 
                [(long)1e1] = 1,
                [(long)1e2] = 25,
                [(long)1e3] = 168,
                [(long)1e4] = 1229,
                [(long)1e5] = 9592,
                [(long)1e6] = 78498,
                [(long)1e7] = 664579,
                [(long)1e8] = 5761455
            };

            /// <summary> Keeps track of the numbers that have been marked us not-primes. </summary>
            private readonly bool[] nonPrime;
            private readonly long sieveSize = 0;

            public PrimeSieve(long size) 
            {
                sieveSize = size;
                nonPrime = new bool[size+1];
            }

            int _primesCount = -1;
            public int PrimesCount {
                get {
                    if (this._primesCount > -1) return this._primesCount;

                    int count = 1; // counting the 2.
                    for (int i = 3; i < this.nonPrime.Length; i+=2) {
                        // if it is not marked as a non-prime, it should be a prime.
                        if (!nonPrime[i]) {
                            count++;
                        }
                    }
                    return this._primesCount = count;
                }
            }

            /// <summary> Compares the count of the resulted primes to historical data. </summary>
            public bool ValidateResults()
            {
                if (historicalData.TryGetValue(this.sieveSize, out int expected))
                    return expected == this.PrimesCount;
                return false;
            }

            /// <summary> Calculate the primes up to the specified limit. </summary>
            public void RunSieve()
            {
                int factor = 3;
                int q = (int) Math.Sqrt(this.sieveSize);

                while (factor < q)
                {
                    for (int num = factor; num <= this.sieveSize; num+=2)
                    {
                        if (!this.nonPrime[num])
                        {
                            factor = num;
                            break;
                        }
                    }

                    // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
                    // We can then step by factor * 2 because every second one is going to be even by definition
                    for (int num = factor * factor; num <= this.sieveSize; num += factor * 2)
                        this.nonPrime[num] = true;

                    factor += 2;
                }
            }

            public void PrintResults(bool showResults, double duration, int passes) {
                if (showResults)
                    Console.Write("2, ");

                int count = 1;
                for (int num = 3; num <= this.sieveSize; num++) {
                    if (!nonPrime[num]) {
                        if (showResults)
                            Console.Write(num + ", ");
                        count++;
                    }
                }
                if (showResults) Console.WriteLine();
                Console.WriteLine($"Passes: {passes}, Time: {duration:0.0000}s, Avg: {passes/duration:n} passes/s, Limit: {this.sieveSize}, Count: {count}, Valid: {ValidateResults()}");
            }
        }

        static void Main()
        {
            var tStart = DateTime.UtcNow;
            var passes = 0;
            PrimeSieve sieve = null;

            while ((DateTime.UtcNow - tStart).TotalSeconds < Duration)
            {
                sieve = new PrimeSieve(SieveSize);
                sieve.RunSieve();
                passes++;
            }

            var tD = DateTime.UtcNow - tStart;
            if (sieve != null)
                sieve.PrintResults(false, tD.TotalSeconds, passes);
        }
    }
}
