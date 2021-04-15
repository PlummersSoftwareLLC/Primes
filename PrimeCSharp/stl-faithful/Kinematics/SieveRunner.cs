using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using PrimeCSharp.Sieves;

namespace PrimeCSharp
{
    class SieveRunner
    {
        const long MillisecondsPerSecond = 1000;

        public static void RunSieve(RunSettings runSettings)
        {
            Console.WriteLine($"Starting ({GetVersionMessage(runSettings)})...");

            if (RunStructs(runSettings))
                return;

            Func<ISieve> createSieve = GetSieveCreator(runSettings);

            long durationLimit = runSettings.SecondsToRun * MillisecondsPerSecond;
            Stopwatch watch = Stopwatch.StartNew();

            var (sieve, passes) = runSettings.MultiThreaded switch
            {
                true => RunMultiThread(createSieve, watch, durationLimit, runSettings.ThreadCount),
                false => RunSingleThread(createSieve, watch, durationLimit),
            };

            watch.Stop();

            if (sieve is not null)
                PrintResults(sieve, passes, watch, runSettings.ShowResults);
            else
                Console.WriteLine("Invalid state after run.");
        }

        private static string GetVersionMessage(RunSettings runSettings)
        {
            string versionMessage = runSettings switch
            {
                { RawParallel: true } => "parallel version",
                { Original: true } => "original",
                { BoolArray: true } => "bool array",
                { InvertedBoolArray: true } => "inverted bool array",
                { DirectInvertedBoolArray: true } => "direct access inverted bool array",
                { RawBits: true } => "raw bits",
                { RawBits32: true } => "raw bits uint",
                { RawBitsDirect: true } => "raw bits direct",
                { ArrayPool: true } => "array pool",
                { ArrayPoolRef: true } => "array pool - ref struct",
                { ArrayPool2Of6: true } => "array pool [2 of 6]",
                { ArrayPool8of30: true } => "array pool [8 of 30]",
                { ArrayPool8of30M: true } => "array pool [8 of 30] with bitmasking",
                { RawBits2Of6: true } => "raw bits [2 of 6]",
                { ArrayPool6P: true } => "parallel array pool [2 of 6]",
                _ => "standard"
            };

            if (runSettings.MultiThreaded)
                versionMessage = $"multithreaded {versionMessage}";

            return versionMessage;
        }

        private static Func<ISieve> GetSieveCreator(RunSettings runSettings)
        {
            return runSettings switch
            {
                { RawParallel: true } => () => new PrimeSieveRawParallel(runSettings.SieveSize, runSettings.PThreadCount),
                { Original: true } => () => new PrimeSieveOriginal(runSettings.SieveSize),
                { BoolArray: true } => () => new PrimeSieveBool(runSettings.SieveSize),
                { InvertedBoolArray: true } => () => new PrimeSieveInvBool(runSettings.SieveSize),
                { DirectInvertedBoolArray: true } => () => new PrimeSieveInvBoolB(runSettings.SieveSize),
                { RawBits: true } => () => new PrimeSieveRawBits(runSettings.SieveSize),
                { RawBits32: true } => () => new PrimeSieveRawBitsInter(runSettings.SieveSize),
                { RawBitsDirect: true } => () => new PrimeSieveRawBitsDirect(runSettings.SieveSize),
                { ArrayPool: true } => () => new PrimeSieveArrayPool(runSettings.SieveSize),
                { ArrayPool2Of6: true } => () => new PrimeSieveArrayPool2Of6(runSettings.SieveSize),
                { ArrayPool8of30: true } => () => new PrimeSieveArrayPool8of30(runSettings.SieveSize),
                { ArrayPool8of30M: true } => () => new PrimeSieveArrayPool8of30M(runSettings.SieveSize),
                { RawBits2Of6: true } => () => new PrimeSieveRawBits2Of6(runSettings.SieveSize),
                { ArrayPool6P: true } => () => new PrimeSieveArrayPool6Par(runSettings.SieveSize, runSettings.PThreadCount),
                _ => () => new PrimeSieve(runSettings.SieveSize),
            };
        }


        private static (ISieve? sieve, int passes) RunSingleThread(Func<ISieve> create, Stopwatch watch, long durationLimit)
        {
            ISieve? sieve = null;
            int passes = 0;

            while (watch.ElapsedMilliseconds < durationLimit)
            {
                sieve = create();
                sieve.Run();
                passes++;
            }

            return (sieve, passes);
        }

        private static (ISieve? sieve, int passes) RunMultiThread(
            Func<ISieve> create,
            Stopwatch watch,
            long durationLimit,
            int threadCount)
        {
            ISieve? sieve;
            ISieve? firstSieve = null;
            int passes = 0;

            if (threadCount <= 0)
                threadCount = Environment.ProcessorCount;

            while (watch.ElapsedMilliseconds < durationLimit)
            {
                Parallel.For(0, threadCount, i =>
                {
                    sieve = create();
                    firstSieve ??= sieve;
                    sieve.Run();
                    if (watch.ElapsedMilliseconds < durationLimit)
                        passes++;
                });
            }

            return (firstSieve, passes);
        }


        private static void PrintResults(ISieve sieve, int passes, Stopwatch watch, bool showResults)
        {
            if (showResults)
            {
                string listing = sieve.GetFoundPrimes()
                    .Select(a => a.ToString())
                    .Aggregate((a, b) => $"{a}, {b}");

                Console.WriteLine(listing);
                Console.WriteLine();
            }

            List<string> results = new();

            results.Add($"Passes: {passes}");
            results.Add($"Time: {watch.Elapsed.TotalSeconds:G3}");
            results.Add($"MS per Loop: {((double)watch.ElapsedMilliseconds / passes):F6}");
            results.Add($"Sieve Size: {sieve.SieveSize}");
            results.Add($"Primes Found: {sieve.CountPrimes()}");
            results.Add($"Valid: {PrimeData.IsCountCorrect(sieve.SieveSize, sieve.CountPrimes())?.ToString() ?? "Unable to determine"}");

            if (sieve.ClearCount > 0)
            {
                results.Add($"Clear Count: {sieve.ClearCount}");
            }

            Console.WriteLine(results.Aggregate((a, b) => $"{a}, {b}"));
        }

        #region Code for running struct versions.
        private static bool RunStructs(RunSettings runSettings)
        {
            if (runSettings.ArrayPoolRef == false)
                return false;

            long durationLimit = runSettings.SecondsToRun * MillisecondsPerSecond;
            Stopwatch watch = Stopwatch.StartNew();

            if (runSettings.MultiThreaded)
                RunMultiThreadStruct(runSettings.SieveSize, watch, durationLimit, runSettings.ThreadCount);
            else
                RunSingleThreadStruct(runSettings.SieveSize, watch, durationLimit);

            return true;
        }

        private static bool RunSingleThreadStruct(int sieveSize, Stopwatch watch, long durationLimit)
        {
            int passes = 0;
            PrimeSieveArrayPoolRef sieve = new PrimeSieveArrayPoolRef(sieveSize);

            while (watch.ElapsedMilliseconds < durationLimit)
            {
                sieve.Run();
                passes++;
                sieve = new PrimeSieveArrayPoolRef(sieveSize);
            }

            watch.Stop();

            sieve.RunSieve();

            PrintStructResults(passes, watch, sieveSize, sieve.CountPrimes());

            sieve.Dispose();

            return true;
        }

        private static bool RunMultiThreadStruct(
            int sieveSize,
            Stopwatch watch,
            long durationLimit,
            int threadCount)
        {
            int passes = 0;
            int count = 0;

            if (threadCount <= 0)
                threadCount = Environment.ProcessorCount;

            while (watch.ElapsedMilliseconds < durationLimit)
            {
                Parallel.For(0, threadCount, i =>
                {
                    var sieve = new PrimeSieveArrayPoolRef(sieveSize);
                    sieve.Run();
                    if (watch.ElapsedMilliseconds < durationLimit)
                        passes++;
                    else
                        count = sieve.CountPrimes();
                });
            }

            watch.Stop();

            PrintStructResults(passes, watch, sieveSize, count);

            return true;
        }

        private static void PrintStructResults(int passes, Stopwatch watch, int size, int count)
        {
            List<string> results = new();

            results.Add($"Passes: {passes}");
            results.Add($"Time: {watch.Elapsed.TotalSeconds:G3}");
            results.Add($"MS per Loop: {((double)watch.ElapsedMilliseconds / passes):F6}");
            results.Add($"Sieve Size: {size}");
            results.Add($"Primes Found: {count}");
            results.Add($"Valid: {PrimeData.IsCountCorrect(size, count)?.ToString() ?? "Unable to determine"}");

            Console.WriteLine(results.Aggregate((a, b) => $"{a}, {b}"));
        }
        #endregion Code for running struct versions.
    }
}
