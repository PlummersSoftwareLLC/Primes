// ---------------------------------------------------------------------------
// PrimeCS.cs : Dave's Garage Prime Sieve in C#
// ---------------------------------------------------------------------------

using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.CompilerServices;

namespace PrimeSieveCS
{
    class PrimeCS
    {
        class prime_sieve
        {
            private int sieveSize = 0;
            private int halfLimit;
            private const int shiftAmount = 5;

            private readonly UInt32[] halfbits;

            private static Dictionary<int, int> myDict = new Dictionary<int, int>
            {
                {          10 , 4 },                // Historical data for validating our results - the number of primes
                {         100 , 25 },               // to be found under some limit, such as 168 primes under 1000
                {       1_000 , 168 },
                {      10_000 , 1229 },
                {     100_000 , 9592 },
                {   1_000_000 , 78498 },
                {  10_000_000 , 664579 },
                { 100_000_000 , 5761455 }
            };

            public prime_sieve(int size)
            {
                sieveSize = size;
                halfLimit = (size + 1) / 2;
                
                int arraySize = (halfLimit >> shiftAmount) + 1;
                halfbits = ArrayPool<UInt32>.Shared.Rent(arraySize);
                
                // the buffer we get from the pool may not be zero-initialized
                for (int i = 0; i < arraySize; i++)
                    halfbits[i] = 0;
            }

            public int countOfPrimes
            {
                get
                {
                    var bits = halfbits.AsSpan();

                    int count = 0;
                    for (int num = 1; num <= this.halfLimit; num++)
                        if ((bits[num >> shiftAmount] & (1U << (num))) == 0)
                            count++;
                    return count;
                }
            }

            public bool containsValidResults
            {
                get
                {
                    return (myDict.ContainsKey(this.sieveSize) && myDict[this.sieveSize] == this.countOfPrimes);
                }
            }

            // primeSieve
            // 
            // Calculate the primes up to the specified limit

            [MethodImpl(MethodImplOptions.AggressiveOptimization)]
            public void runSieve()
            {
                var bits = halfbits.AsSpan();
                var factor = 3;
                var halfFactor = factor >> 1;
                var halfRoot = ((int)(Math.Sqrt(this.sieveSize) + 1)) >> 1;

                // We ignore even numbers by using values that track half of the actuals, and the only
                // number we keep in original form is the prime factor we're walking through the sieve

                while (halfFactor <= halfRoot)
                {
                    // Scan for the next unset bit which means it is a prime factor

                    while (halfFactor <= halfRoot)
                    {
                        if ((bits[halfFactor >> shiftAmount] & (1U << (halfFactor))) == 0U)
                            break;
                        halfFactor++;
                    }

                    factor = (halfFactor << 1) + 1;

                    // Mark off all multiples starting with the factor's square up to the square root of the limit

                    for (var index = (factor * factor) >> 1; index < halfLimit; index += factor)
                        bits[index >> shiftAmount] |= (1U << (index));

                    halfFactor++;
                }

                // Return the memory to the pool

                ArrayPool<UInt32>.Shared.Return(halfbits);
            }

            public void printResults(bool showResults, double duration, int passes)
            {
                var bits = halfbits.AsSpan();

                if (showResults)
                    Console.Write("2, ");

                int count = 1;
                for (int num = 3; num <= this.sieveSize; num += 2)
                {
                    if ((bits[(num / 2) >> shiftAmount] & (1U << (num / 2))) == 0)
                    {
                        if (showResults)
                            Console.Write(num + ", ");
                        count++;
                    }
                }
                if (showResults)
                    Console.WriteLine("");

                CultureInfo.CurrentCulture = new CultureInfo("en_US", false);

                Console.WriteLine("Passes: " + passes + ", Time: " + duration + ", Avg: " + (duration / passes) + ", Limit: " + this.sieveSize + ", Count: " + count + ", Valid: " + this.containsValidResults);

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
