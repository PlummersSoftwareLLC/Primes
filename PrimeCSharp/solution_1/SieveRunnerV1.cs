using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using PrimeCSharp.Config;
using PrimeCSharp.V1Sieves;

namespace PrimeCSharp
{
    class SieveRunnerV1
    {
        const long MillisecondsPerSecond = 1000;

        public static void RunSieve(SettingsV1 runSettings)
        {
            Console.WriteLine($"@Kinematics: Starting ({GetVersionMessage(runSettings)})...");

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
                PrintResults(sieve, passes, watch, runSettings);
            else
                Console.WriteLine("Invalid state after run.");
        }

        public static void RunAllSieves(SettingsV1 options)
        {
            SettingsV1 tmpOptions = options.CopyOptions();

            tmpOptions.Original = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.Standard = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.BoolArray = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.InvertedBoolArray = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.DirectInvertedBoolArray = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.RawBits = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.RawBits32 = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.RawBitsDirect = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.RawBits2Of6 = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.RawParallel = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.ArrayPool = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.ArrayPool2Of6 = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.ArrayPool6P = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.ArrayPool8of30 = true;
            RunSieve(tmpOptions);
            Console.WriteLine();
            tmpOptions = options.CopyOptions();

            tmpOptions.ArrayPool8of30M = true;
            RunSieve(tmpOptions);
        }

        private static string GetVersionMessage(SettingsV1 runSettings)
        {
            string versionMessage = runSettings switch
            {
                { Original: true } => "original",
                { BoolArray: true } => "bool array",
                { InvertedBoolArray: true } => "inverted bool array",
                { DirectInvertedBoolArray: true } => "direct access inverted bool array",
                { RawBits: true } => "raw bits",
                { RawBits32: true } => "raw bits uint",
                { RawBitsDirect: true } => "raw bits direct",
                { RawBits2Of6: true } => "raw bits [2 of 6]",
                { RawParallel: true } => "raw bits parallel version",
                { ArrayPool: true } => "array pool",
                { ArrayPoolRef: true } => "array pool - ref struct",
                { ArrayPool2Of6: true } => "array pool [2 of 6]",
                { ArrayPool6P: true } => "parallel array pool [2 of 6]",
                { ArrayPool8of30: true } => "array pool [8 of 30]",
                { ArrayPool8of30M: true } => "array pool [8 of 30] with bitmasking",
                _ => "standard"
            };

            if (runSettings.MultiThreaded)
                versionMessage = $"multithreaded {versionMessage}";

            return versionMessage;
        }

        private static Func<ISieve> GetSieveCreator(SettingsV1 runSettings)
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
                    {
                        Interlocked.Increment(ref passes);
                    }
                });
            }

            return (firstSieve, passes);
        }


        private static void PrintResults(ISieve sieve, int passes, Stopwatch watch, SettingsV1 runSettings)
        {
            if (runSettings.ShowResults)
            {
                string listing = sieve.GetFoundPrimes()
                    .Select(a => a.ToString())
                    .Aggregate((a, b) => $"{a}, {b}");

                Console.WriteLine(listing);
                Console.WriteLine();
            }

            int threads = 1;
            if (runSettings.MultiThreaded)
            {
                threads = runSettings.ThreadCount == 0 ? Environment.ProcessorCount : runSettings.ThreadCount;
            }

            int pthreads = 1;
            if (sieve.IsParallel)
            {
                pthreads = runSettings.PThreadCount == 0 ? Environment.ProcessorCount : runSettings.PThreadCount;
            }

            List<string> results = new();

            results.Add($"Passes: {passes}");
            results.Add($"Time: {watch.Elapsed.TotalSeconds:G6} s");
            results.Add($"Per Loop: {((double)watch.ElapsedMilliseconds / passes):F6} ms");
            results.Add($"Sieve Size: {sieve.SieveSize}");
            results.Add($"Thread Count: {threads}");
            if (sieve.IsParallel)
            {
                results.Add($"Parallel Thread Count: {pthreads}");
            }
            results.Add($"Primes Found: {sieve.CountPrimes()}");
            results.Add($"Valid: {PrimeData.IsCountCorrect(sieve.SieveSize, sieve.CountPrimes())?.ToString() ?? "Unable to determine"}");

            if (sieve.ClearCount > 0)
            {
                results.Add($"Clear Count: {sieve.ClearCount}");
            }

            Console.WriteLine(results.Aggregate((a, b) => $"{a}, {b}"));

            // official syntax:
            // kinematics_<sieve_tag>;<pass_count>;<runtime>;<pthread_count>
            
            int totalThreads = threads * pthreads;
            string bitsSuffix = sieve.BitsPerPrime.HasValue ? $",bits={sieve.BitsPerPrime}" : string.Empty;

            Console.WriteLine($"kinematics_{sieve.QuickName};{passes};{watch.Elapsed.TotalSeconds:G6};{totalThreads};algorithm={sieve.AlgorithmType},faithful=yes{bitsSuffix}");
        }

        #region Code for running struct versions.
        private static bool RunStructs(SettingsV1 runSettings)
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
