using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;

namespace PrimeSieveCS
{
    public interface ISieve
    {
        string Name { get; }
        uint SieveSize { get; }
        IEnumerable<uint> EnumeratePrimes();
    }

    /// <summary>
    /// This is just a workaround to avoid an indirect function call to construct the sieve.
    /// This can be replaced with a static function call when we get statics in interfaces.
    /// </summary>
    interface ISieveRunner
    {
        ISieve RunSieve(uint sieveSize);
    }

    static class PrimeCS
    {
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

        static bool ContainsValidResults(ISieve sieve)
        {
            return validationDict.ContainsKey(sieve.SieveSize) &&
                   validationDict[sieve.SieveSize] == sieve.EnumeratePrimes().Count();
        }

        static void Main()
        {
            CultureInfo.CurrentCulture = new CultureInfo("en-US", false);

            const int sieveSize = 1_000_000;

            RunSieve(default(SieveDenseAndSparseRunner), sieveSize);
            RunSieve(default(SieveStride8Runner), sieveSize);
            RunSieve(default(SieveStride8Blocks16kRunner), sieveSize);
            RunSieve(default(SieveStride8Blocks32kRunner), sieveSize);
            RunSieve(default(SieveStride8Blocks64kRunner), sieveSize);
        }

        static void RunSieve<TRunner>(TRunner runner, uint sieveSize) where TRunner : ISieveRunner
        {
            var sw = Stopwatch.StartNew();

            //warmup - 5 seconds permitted
            while (sw.Elapsed.TotalSeconds < 5)
                runner.RunSieve(sieveSize);

            //Forcing a GC to give us as much space on the heap as possible (dont do this in real code).
            //It's not part of the measurement either way.
            GC.Collect(0, GCCollectionMode.Forced, true, true);

            //running the dragrace
            var passes = 0;
            ISieve sieve = default;
            sw.Restart();
            while (sw.Elapsed.TotalSeconds < 5)
            {
                sieve = runner.RunSieve(sieveSize);
                passes++;
            }
            sw.Stop();

            if (sieve != null)
                PrintResults(sieve, false, sw.Elapsed.TotalSeconds, passes);
        }

        static void PrintResults(ISieve sieve, bool showResults, double duration, int passes)
        {
            if (showResults)
            {
                Console.WriteLine(string.Join(", ", sieve.EnumeratePrimes()));
                Console.WriteLine();
            }

            Console.WriteLine("Passes: " + passes + ", Time: " + duration + ", Avg: " + (duration / passes) + ", Limit: " + sieve.SieveSize + ", Count: " + sieve.EnumeratePrimes().Count() + ", Valid: " + ContainsValidResults(sieve));

            // Following 2 lines added by rbergen to conform to drag race output format
            Console.WriteLine();
            Console.WriteLine($"{sieve.Name};{passes};{duration:G6};1;algorithm=base,faithful=yes,bits=1");
        }
    }
}
