// ---------------------------------------------------------------------------
// PrimeCS.cs : Dave's Garage Prime Sieve in C++
// ---------------------------------------------------------------------------

using System;
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

            unsafe static void ClearBitsWithRolMulti(ulong* ptr, uint start, uint factor, uint limit)
            {
                ulong rolling_mask = 1UL << (int)(start);
                uint offset = start % 64;
                for (uint index = start / 64; index < limit / 64 + 1; index++)
                {
                    var ptroffset = ptr + index;
                    var segment = ptroffset[0];
                    do
                    {
                        segment |= rolling_mask;
                        rolling_mask = BitOperations.RotateLeft(rolling_mask, (int)factor);
                        offset += factor;
                    } while (offset < 64);
                    ptroffset[0] = segment;
                    offset -= 64;
                }
            }

            unsafe static void ClearBitsDefault(ulong* ptr, uint start, uint factor, uint limit)
            {
                var i0 = start;
                var i1 = start + factor;
                var i2 = start + factor * 2;
                var i3 = start + factor * 3;

                var factor4 = factor * 4;
                while (i3 < limit)
                {
                    ptr[i0 / 64] |= 1ul << (int)(i0 % 64);
                    ptr[i1 / 64] |= 1ul << (int)(i1 % 64);
                    ptr[i2 / 64] |= 1ul << (int)(i2 % 64);
                    ptr[i3 / 64] |= 1ul << (int)(i3 % 64);

                    i0 += factor4;
                    i1 += factor4;
                    i2 += factor4;
                    i3 += factor4;
                }

                while (i0 < limit)
                {
                    ptr[i0 / 64] |= 1ul << (int)(i0 % 64);
                    i0 += factor;
                }
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
                fixed (ulong* ptr = bits)
                    while (true)
                    {
                        // Scan for the next unset bit which means it is a prime factor
                        var segment = ptr[halfFactor / 64];
                        var offset = halfFactor % 64;
                        segment = ~segment; //since we only have access to TrailingZeroCount, we have to flip all the bits
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

                        //marking with a rolling mask if we can get enough bits in the ulong, 20 seems to be optimal
                        if (halfFactor < 20)
                        {
                            ClearBitsWithRolMulti(ptr, (factor * factor) / 2, factor, halfLimit);
                        }
                        else
                        {
                            ClearBitsDefault(ptr, (factor * factor) / 2, factor, halfLimit);
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
            var wStart = DateTime.UtcNow;
            while ((DateTime.UtcNow - wStart).TotalSeconds < 3)
                new prime_sieve(sievesize).runSieve();
            GC.Collect();

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
