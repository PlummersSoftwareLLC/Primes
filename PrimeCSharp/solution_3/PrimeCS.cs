// ---------------------------------------------------------------------------
// PrimeCS.cs : Dave's Garage Prime Sieve in C#
// ---------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace PrimeSieveCS
{
    class PrimeCS
    {
        private static readonly Dictionary<ulong, ulong> myDict = new Dictionary<ulong, ulong>
        {
            [10] = 4,                 // Historical data for validating our results - the number of primes
            [100] = 25,               // to be found under some limit, such as 168 primes under 1000
            [1000] = 168,
            [10000] = 1229,
            [100000] = 9592,
            [1000000] = 78498,
            [10000000] = 664579,
            [100000000] = 5761455,
            [1000000000] = 50847534,
            [10000000000] = 455052511,
        };

        class prime_sieve
        {
            private readonly ulong sieveSize;
            private readonly byte[] rawbits;

            [MethodImpl(MethodImplOptions.AggressiveOptimization)]
            public prime_sieve(ulong n)
            {
                sieveSize = n;
                rawbits = GC.AllocateUninitializedArray<byte>((int)((n / 8) + 1), pinned: true);
                rawbits.AsSpan().Fill(0xFF);
            }

            public ulong countPrimes()
            {
                var sieveSize = this.sieveSize;
                var rawbits = this.rawbits;

                // hoist null check
                _ = getrawbits(rawbits, 0);

                ulong count = (sieveSize >= 2) ? 1UL : 0UL;
                for (ulong i = 3; i < sieveSize; i+=2)
                    if (GetBit(rawbits, i))
                        count++;
                return count;
            }

            private bool validateResults()
            {
                return myDict.TryGetValue(sieveSize, out ulong sieveResult) && (sieveResult == countPrimes());
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            private static bool GetBit(byte[] rawbits, ulong index)
            {
                Debug.Assert((index % 2) != 0);
                index /= 2;
                return (getrawbits(rawbits, index / 8U) & (1u << (int)(index % 8))) != 0;
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            private static void ClearBit(byte[] rawbits, ulong index)
            {
                Debug.Assert((index % 2) != 0);
                index /= 2;
                getrawbits(rawbits, index / 8) &= (byte)~(1u << (int)(index % 8));
            }

            // primeSieve
            // 
            // Calculate the primes up to the specified limit

            [MethodImpl(MethodImplOptions.AggressiveOptimization)]
            public void runSieve()
            {
                var sieveSize = this.sieveSize;
                var rawbits = this.rawbits;

                // hoist null check
                _ = getrawbits(rawbits, 0);

                ulong factor = 3;
                ulong q = (ulong)Math.Sqrt(sieveSize);

                while (factor <= q)
                {
                    for (ulong num = factor; num < sieveSize; num += 2)
                    {
                        if (GetBit(rawbits, num))
                        {
                            factor = num;
                            break;
                        }
                    }

                    // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
                    // We can then step by factor * 2 because every second one is going to be even by definition

                    for (ulong num = factor * factor; num < sieveSize; num += factor * 2)
                        ClearBit(rawbits, num);

                    factor += 2;
                }
            }

            public void printResults(bool showResults, double duration, ulong passes)
            {
                var sieveSize = this.sieveSize;
                var rawbits = this.rawbits;

                // hoist null check
                _ = getrawbits(rawbits, 0);

                if (showResults)
                    Console.Write("2, ");

                ulong count = (sieveSize >= 2) ? 1UL : 0UL;
                for (ulong num = 3; num <= sieveSize; num += 2)
                {
                    if (GetBit(rawbits, num))
                    {
                        if (showResults)
                            Console.Write(num + ", ");
                        count++;
                    }
                }

                if (showResults)
                    Console.WriteLine();
                Console.WriteLine($"Passes: {passes}, Time: {duration}, Avg: {duration / passes}, Limit: {sieveSize}, Count: {countPrimes()}, Valid: {validateResults()}");

                Console.WriteLine();
                Console.WriteLine($"tannergooding;{passes};{duration};1;algorithm=base,faithful=yes,bits=1", passes, duration);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            private static ref byte getrawbits(byte[] rawbits, ulong index)
            {
                return ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(rawbits), (nint)index);
            }
        }

        static void Main(string[] args)
        {
            const ulong SieveSize = 1000000;
            const long MillisecondsPerSecond = 1000;
            const long MicrosecondsPerSecond = 1000000;

            ulong passes = 0;
            prime_sieve sieve = null;

            var stopwatch = Stopwatch.StartNew();

            while (stopwatch.ElapsedMilliseconds < (5 * MillisecondsPerSecond))
            {
                sieve = new prime_sieve(SieveSize);
                sieve.runSieve();
                passes++;
            }
            stopwatch.Stop();

            if (sieve != null)
                sieve.printResults(false, (stopwatch.Elapsed.TotalSeconds * MicrosecondsPerSecond) / SieveSize, passes);
        }
    }
}
