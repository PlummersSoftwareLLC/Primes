// ---------------------------------------------------------------------------
// PrimeCS.cs : Dave's Garage Prime Sieve in C#
// ---------------------------------------------------------------------------

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;

namespace PrimeSieveCS
{
    class PrimeCS
    {
        class prime_sieve
        {
            private int sieveSize = 0;
            private BitArray bitArray;
            private Dictionary<int, int> myDict = new()
            {
                { 10 , 1 },                 // Historical data for validating our results - the number of primes
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
                bitArray = new BitArray(sieveSize, true);
            }

            public int countPrimes()
            {
                int count = 0;
                for (int i = 0; i < bitArray.Count; i++)
                    if (GetBit(i))
                        count++;
                return count;
            }

            public bool validateResults()
            {
                if (myDict.TryGetValue(sieveSize, out var properCount))
                    return countPrimes() == properCount;

                return false;
            }

            private bool GetBit(int index)
            {
                if (index % 2 == 0)
                    return false;

                return bitArray[index];
            }

            private void ClearBit(int index)
            {
                if (index % 2 == 0)
                {
                    Console.WriteLine("You are setting even bits, which is sub-optimal");
                    return;
                }
                bitArray[index] = false;
            }

            // primeSieve
            //
            // Calculate the primes up to the specified limit

            public void runSieve()
            {
                int factor = 3;
                int q = (int) Math.Sqrt(sieveSize);

                while (factor < q)
                {
                    for (int num = factor; num <= sieveSize; num++)
                    {
                        if (GetBit(num))
                        {
                            factor = num;
                            break;
                        }
                    }

                    // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
                    // We can then step by factor * 2 because every second one is going to be even by definition

                    for (int num = factor * 3; num <= sieveSize; num += factor * 2)
                        ClearBit(num);

                    factor += 2;
                }
            }

            public void printResults(bool showResults, double duration, int passes)
            {
                if (showResults)
                    Console.Write("2, ");

                int count = 1;
                for (int num = 3; num <= sieveSize; num++)
                {
                    if (GetBit(num))
                    {
                        if (showResults)
                            Console.Write($"{num}, ");

                        count++;
                    }
                }
                if (showResults)
                    Console.WriteLine(string.Empty);
                Console.WriteLine($"Passes: {passes}, Time: {duration}, Avg: {(duration / passes)}, Limit: {sieveSize}, Count: {count}, Valid: {validateResults()}");
            }
        }

        static void Main(string[] args)
        {
            var passes = 0;
            prime_sieve sieve = null;

            Stopwatch stopwatch = Stopwatch.StartNew();
            while (stopwatch.ElapsedMilliseconds < 10000)
            {
                sieve = new prime_sieve(1000000);
                sieve.runSieve();
                passes++;
            }

            var tD = stopwatch.Elapsed;
            if (sieve != null)
                sieve.printResults(false, tD.TotalSeconds, passes);
        }
    }
}
