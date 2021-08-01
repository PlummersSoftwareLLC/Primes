﻿// ---------------------------------------------------------------------------
// PrimeCS.cs : Dave's Garage Prime Sieve in C++
// ---------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Numerics;
using System.Runtime.CompilerServices;

namespace PrimeSieveCS
{
    class PrimeCS
    {
        class PrimeSieve
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

            public PrimeSieve(uint size)
            {
                const int wordBits = sizeof(ulong) * 8;
                
                sieveSize = size;
                halfLimit = (size + 1) / 2;
                bits = new ulong[(int)(halfLimit / wordBits + 1)];
            }

            public IEnumerable<uint> EnumeratePrimes()
            {
                yield return 2;
                for (uint num = 3; num <= sieveSize; num += 2)
                    if ((bits[(num / 2) / 64] & (1UL << (int)(num / 2))) == 0)
                        yield return num;
            }

            public bool ContainsValidResults
            {
                get
                {
                    return validationDict.ContainsKey(sieveSize) && validationDict[sieveSize] == EnumeratePrimes().Count();
                }
            }

            unsafe static void ClearBitsDense(ulong* ptr, uint start, int factor, uint limit)
            {
                Debug.Assert(factor < 64, "factor cant be bigger than 63, that will cause incorrect calcualtions. This is optimized for lower factors");

                //Performance: we want factor and offset as an int so we can dodge a SUB in the while comparison in the inner loop

                var ptrStart = ptr + start / 64;
                var ptrEnd   = ptr + limit / 64;

                ulong rollingMask = 1UL << (int)(start);
                int offset = (int)(64 - start % 64);
                while (ptrStart <= ptrEnd)
                {
                    var segment = ptrStart[0];
                    do
                    {
                        segment |= rollingMask;
                        rollingMask = BitOperations.RotateLeft(rollingMask, factor);
                        offset -= factor;
                    } while (offset > 0);
                    offset += 64;
                    ptrStart[0] = segment;
                    ptrStart++;
                }
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            unsafe static void ClearBitsSparse(ulong* ptr, uint start, uint factor, uint limit)
            {
                for (uint index = start; index < limit; index += factor)
                {
                    ptr[index / 64] |= 1UL << (int)(index % 64);
                }
            }

            /// <summary>
            /// Unrolled version of ClearBitsSparse.
            /// 
            /// Provided by mike-barber.
            /// </summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            unsafe static void ClearBitsSparseUnrolled4(ulong* ptr, uint start, uint factor, uint limit)
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
            unsafe public void RunSieve()
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

                        //scan finnished - restoring factor
                        halfFactor += (uint)jump;
                        factor = (halfFactor << 1) + 1;
                        halfFactor++;

                        if (halfFactor > halfRoot) break;

                        //marking with a rolling mask if we can get enough bits in the ulong, 20 seems to be optimal
                        if (halfFactor < 20)
                        {
                            ClearBitsDense(ptr, (factor * factor) / 2, (int)factor, halfLimit);
                        }
                        else
                        {
                            ClearBitsSparseUnrolled4(ptr, (factor * factor) / 2, factor, halfLimit);
                        }
                    }
            }
        }

        static void Main(string[] args)
        {
            //setup
            const int sieveSize = 1_000_000;
            CultureInfo.CurrentCulture = new CultureInfo("en-US", false);

            //warmup - 5 seconds permitted
            var wStart = DateTime.UtcNow;
            while ((DateTime.UtcNow - wStart).TotalSeconds < 5)
                new PrimeSieve(sieveSize).RunSieve();
            GC.Collect();

            //running the dragrace
            var tStart = DateTime.UtcNow;
            var passes = 0;
            PrimeSieve sieve = null;

            while ((DateTime.UtcNow - tStart).TotalSeconds < 5)
            {
                sieve = new PrimeSieve(sieveSize);
                sieve.RunSieve();
                passes++;
            }

            var tD = DateTime.UtcNow - tStart;

            if (sieve != null)
                PrintResults(sieve, false, tD.TotalSeconds, passes);
        }

        static void PrintResults(PrimeSieve sieve, bool showResults, double duration, int passes)
        {
            if (showResults)
            {
                Console.WriteLine(string.Join(", ", sieve.EnumeratePrimes()));
                Console.WriteLine();
            }

            Console.WriteLine("Passes: " + passes + ", Time: " + duration + ", Avg: " + (duration / passes) + ", Limit: " + sieve.sieveSize + ", Count: " + sieve.EnumeratePrimes().Count() + ", Valid: " + sieve.ContainsValidResults);

            // Following 2 lines added by rbergen to conform to drag race output format
            Console.WriteLine();
            Console.WriteLine($"italytoast;{passes};{duration:G6};1;algorithm=base,faithful=yes,bits=1");
        }
    }
}
