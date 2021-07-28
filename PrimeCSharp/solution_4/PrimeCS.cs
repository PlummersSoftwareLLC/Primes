// ---------------------------------------------------------------------------
// PrimeCS.cs : Dave's Garage Prime Sieve in C++
// ---------------------------------------------------------------------------

using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Numerics;
using System.Runtime.CompilerServices;

namespace PrimeSieveCS
{
    class PrimeCS
    {
        class prime_sieve
        {
            public readonly uint sieveSize = 0;
            readonly uint halfLimit;
            readonly ulong[] bits;

            static Dictionary<uint, int> validationDict = new Dictionary<uint, int>
            {
                {          10 , 4       },// Historical data for validating our results - the number of primes
                {         100 , 25      },// to be found under some limit, such as 168 primes under 1000
                {       1_000 , 168     },
                {      10_000 , 1229    },
                {     100_000 , 9592    },
                {   1_000_000 , 78498   },
                {  10_000_000 , 664579  },
                { 100_000_000 , 5761455 }
            };

            public prime_sieve(uint size)
            {
                sieveSize = size;
                halfLimit = (size + 1) / 2;
                bits = new ulong[(int)(halfLimit / sizeof(ulong) + 1)];
            }

            public IEnumerable<uint> EnumeratePrimes()
            {
                yield return 2;
                for (uint num = 3; num <= sieveSize; num += 2)
                    if ((bits[(num / 2) / 64] & (1UL << (int)(num / 2))) == 0)
                        yield return num;
            }

            public bool containsValidResults
            {
                get
                {
                    return validationDict.ContainsKey(sieveSize) && validationDict[sieveSize] == EnumeratePrimes().Count();
                }
            }

            static ulong CreateMarkingMask(uint factor)
            {
                ulong mask = 0;
                for (uint i = 0; i < 64; i += factor)
                {
                    mask |= 1UL << (int)i;
                }
                return mask;
            }

            // primeSieve
            // 
            // Calculate the primes up to the specified limit

            [MethodImpl(MethodImplOptions.AggressiveOptimization)]
            unsafe public void runSieve()
            {
                uint factor = 3;
                uint halfFactor = factor >> 1;
                uint halfRoot = ((uint)(Math.Sqrt(sieveSize) + 1)) >> 1;

                // We ignore even numbers by using values that track half of the actuals, and the only
                // number we keep in original form is the prime factor we're walking through the sieve
                fixed(ulong* ptr = bits)
                    while (true)
                    {
                        // Scan for the next unset bit which means it is a prime factor
                        var segment = ptr[halfFactor / 64];
                        var offset = halfFactor % 64;
                        segment ^= 0xFFFFFFFFFFFFFFFF; //since we only have access to TrailingZeroCount, we have to flip all the bits
                        segment >>= (int)offset;
                        var jump = BitOperations.TrailingZeroCount(segment);
                        if (jump == 64)
                        {
                            halfFactor += 64 - offset;
                            continue;
                        }

                        halfFactor += (uint)jump;
                        factor = (halfFactor << 1) + 1;
                        halfFactor++;

                        if (halfFactor > halfRoot) break;

                        //marking with a mask if we can get enough bits in the ulong, 18 seems to be optimal
                        if (halfFactor <= 18)
                        {
                            // Mark off all multiples starting with the factor's square up to the square root of the limit
                            ulong mask = CreateMarkingMask(factor);
                            var start = (factor * factor) / 2;
                            var offset2 = start % 64;
                            for (uint index = start / 64; index < halfLimit / 64 + 1; index++)
                            {
                                var ormask = mask << (int)offset2;
                                ptr[index] |= ormask;
                                var pop = (uint)BitOperations.PopCount(ormask);
                                offset2 += pop * factor - 64;
                            }
                        }
                        else
                        {
                            //Mark off all multiples starting with the factor's square up to the square root of the limit
                            for (uint index = (factor * factor) >> 1; index < halfLimit; index += factor)
                            {
                                var ptrmark = ptr + index / 64;
                                ptrmark[0] |= 1UL << (int)(index % 64);
                            }
                        }
                    }
            }
        }

        static void Main(string[] args)
        {
            //setup
            const int sievesize = 1000000;
            CultureInfo.CurrentCulture = new CultureInfo("en-US", false);

            //warmup 
            for (int i = 0; i < 100; i++)
                new prime_sieve(sievesize).runSieve();

            //running the dragrace
            var tStart = DateTime.UtcNow;
            var passes = 0;
            prime_sieve sieve = null;

            while ((DateTime.UtcNow - tStart).TotalSeconds < 5)
            {
                sieve = new prime_sieve(sievesize);
                sieve.runSieve();
                passes++;
            }

            var tD = DateTime.UtcNow - tStart;
            
            if (sieve != null)
                printResults(sieve, false, tD.TotalSeconds, passes);
        }

        static void printResults(prime_sieve sieve, bool showResults, double duration, int passes)
        {
            if (showResults)
            {
                Console.WriteLine(string.Join(", ", sieve.EnumeratePrimes()));
                Console.WriteLine();
            }

            Console.WriteLine("Passes: " + passes + ", Time: " + duration + ", Avg: " + (duration / passes) + ", Limit: " + sieve.sieveSize + ", Count: " + sieve.EnumeratePrimes().Count() + ", Valid: " + sieve.containsValidResults);

            // Following 2 lines added by rbergen to conform to drag race output format
            Console.WriteLine();
            Console.WriteLine($"italytoast;{passes};{duration:G6};1;algorithm=base,faithful=yes,bits=1");
        }
    }
}
