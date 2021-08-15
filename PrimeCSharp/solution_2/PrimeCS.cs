// ---------------------------------------------------------------------------
// PrimeCS.cs : Dave's Garage Prime Sieve in C#
// ---------------------------------------------------------------------------

using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;

namespace PrimeSieveCS
{
    class PrimeCS
    {
        class prime_sieve
        {
            private readonly int sieveSize = 0;
            private readonly BitArray bitArray; //making it readonly so we tell the compiler that the variable reference cant change. around 5% increase in performance
            private Dictionary<int, int> myDict = new Dictionary<int, int> 
            { 
                { 10 , 4 },                 // Historical data for validating our results - the number of primes
                { 100 , 25 },               // to be found under some limit, such as 168 primes under 1000
                { 1000 , 168 },
                { 10000 , 1229 },
                { 100000 , 9592 },
                { 1000000 , 78498 },
                { 10000000 , 664579 },
                { 100000000 , 5761455 } 
            };

            public prime_sieve(int size) 
            {
                sieveSize = size;
                bitArray = new BitArray(((this.sieveSize + 1) / 2), true);
            }

            public int countPrimes()
            {
                int count = 0;
                for (int i = 0; i < this.bitArray.Count; i++)
                    if (bitArray[i])
                        count++;
                return count;
            }

            public bool validateResults()
            {
                if (myDict.ContainsKey(this.sieveSize))
                    return this.myDict[this.sieveSize] == this.countPrimes();
                return false;
            }

            private bool GetBit(int index)
            {
                if (index % 2 == 0)
                    return false;
                return bitArray[index / 2];
            }

            // primeSieve
            // 
            // Calculate the primes up to the specified limit

            public void runSieve()
            {
                int factor = 3;
                int q = (int) Math.Sqrt(this.sieveSize);

                while (factor < q)
                {
                    for (int num = factor / 2; num <= this.bitArray.Count; num++)
                    {
                        if (bitArray[num])
                        {
                            factor = num * 2 + 1;
                            break;
                        }
                    }

                    // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
                    // We can then step by factor * 2 because every second one is going to be even by definition.
                    // Note that bitArray is only storing odd numbers. That means an increment of "num" by "factor" is actually an increment of 2 * "factor"

                    for (int num = factor * 3 / 2; num < this.bitArray.Count; num += factor)
                        this.bitArray[num] = false;

                    factor += 2;
                }
            }

            public void printResults(bool showResults, double duration, int passes)
            {
                if (showResults)
                    Console.Write("2, ");

                int count = 1;
                for (int num = 3; num <= this.sieveSize; num++)
                {
                    if (GetBit(num))
                    {
                        if (showResults)
                            Console.Write(num + ", ");
                        count++;
                    }
                }
                if (showResults)
                    Console.WriteLine("");

                CultureInfo.CurrentCulture = new CultureInfo("en_US", false);

                Console.WriteLine("Passes: " + passes + ", Time: " + duration + ", Avg: " + (duration / passes) + ", Limit: " + this.sieveSize + ", Count: " + count + ", Valid: " + validateResults());
            
                // Following 2 lines added by rbergen to conform to drag race output format
                Console.WriteLine();
                Console.WriteLine($"davepl;{passes};{duration:G6};1;algorithm=base,faithful=yes,bits=1");
            }
        }

        static void Main(string[] args)
        {
            CultureInfo.CurrentCulture = new CultureInfo("en-US", false);

            var tStart = DateTime.UtcNow;
            var passes = 0;
            prime_sieve sieve = null;

            while ((DateTime.UtcNow - tStart).TotalSeconds < 5)
            {
                sieve = new prime_sieve(1000000);
                sieve.runSieve();
                passes++;
            }

            var tD = DateTime.UtcNow - tStart;
            if (sieve != null)
                sieve.printResults(false, tD.TotalSeconds, passes);
        }
    }
}
