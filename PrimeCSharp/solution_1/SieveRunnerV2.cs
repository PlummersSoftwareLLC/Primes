using PrimeCSharp.Config;
using PrimeCSharp.SieveDetails;
using PrimeCSharp.V2Sieves;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace PrimeCSharp
{
    internal class SieveRunnerV2
    {
        const long MillisecondsPerSecond = 1000;
        const int WarmupCount = 10;

        public static void Run(SettingsV2 settings)
        {
            List<SieveProperty> sievesToRun = settings.GetSieves();

            foreach (var sieve in sievesToRun)
            {
                try
                {
                    Run(sieve, settings);
                }
                catch (ArgumentException e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }

        private static void Run(SieveProperty sieve, SettingsV2 settings)
        {
            Func<ISieveRunner> createSieve = GetSieveCreator(sieve, settings)
                ?? throw new ArgumentException($"Unable to run sieve: {sieve}", nameof(sieve));

            var demo = createSieve();

            Console.WriteLine($"@Kinematics: Starting ({demo.Description}{(settings.MultiThreaded ? " (multithreaded)" : "")})...");

            long millisecondsToRun = settings.SecondsToRun * MillisecondsPerSecond;

            Stopwatch watch = new();

            var (completedSieve, passes) = settings.MultiThreaded switch
            {
                false => RunSingleThread(createSieve, watch, millisecondsToRun, settings),
                true => RunMultiThread(createSieve, watch, millisecondsToRun, settings),
            };

            if (completedSieve is null)
            {
                Console.WriteLine("Invalid state after run.");
                return;
            }

            PrintResults(completedSieve, passes, watch, settings);
        }

        private static (ISieveRunner?, int) RunSingleThread(
            Func<ISieveRunner> createSieve, Stopwatch watch, long millisecondsToRun, SettingsV2 settings)
        {
            ISieveRunner? sieve = null;
            int passes = 0;
            Warmup(createSieve);

            watch.Start();

            while (watch.ElapsedMilliseconds < millisecondsToRun)
            {
                sieve = createSieve();
                sieve.Run();
                passes++;
            }

            watch.Stop();
            return (sieve, passes);
        }

        private static (ISieveRunner?, int) RunMultiThread(
            Func<ISieveRunner> createSieve, Stopwatch watch, long millisecondsToRun, SettingsV2 settings)
        {
            ISieveRunner? sieve;
            ISieveRunner? firstSieve = null;
            int passes = 0;
            int threadCount = settings.ThreadCount;
            if (threadCount <= 0)
                threadCount = Environment.ProcessorCount;

            Warmup(createSieve);

            watch.Start();

            while (watch.ElapsedMilliseconds < millisecondsToRun)
            {
                Parallel.For(0, threadCount, i =>
                {
                    sieve = createSieve();
                    firstSieve ??= sieve;
                    sieve.Run();
                    if (watch.ElapsedMilliseconds < millisecondsToRun)
                    {
                        Interlocked.Increment(ref passes);
                    }
                });
            }

            watch.Stop();
            return (firstSieve, passes);
        }

        private static void Warmup(Func<ISieveRunner> createSieve)
        {
            for (int i = 0; i < WarmupCount; i++)
            {
                var sieve = createSieve();
                sieve.Run();
            }
        }

        private static void PrintResults(ISieveRunner sieve, int passes, Stopwatch watch, SettingsV2 settings)
        {
            if (settings.Verbose)
            {
                string listing = sieve.GetFoundPrimes()
                    .Select(a => a.ToString())
                    .Aggregate((a, b) => $"{a}, {b}");

                Console.WriteLine(listing);
                Console.WriteLine();
            }

            int threads = 1;
            if (settings.MultiThreaded)
            {
                threads = settings.ThreadCount == 0 ? Environment.ProcessorCount : settings.ThreadCount;
            }

            int pthreads = 1;
            if (sieve.IsParallel)
            {
                pthreads = settings.PThreadCount == 0 ? Environment.ProcessorCount : settings.PThreadCount;
            }

            List<string> results = new();

            results.Add($"Passes: {passes}");
            results.Add($"Time: {watch.Elapsed.TotalSeconds:G6} s");
            results.Add($"{((double)watch.ElapsedMilliseconds / passes):F6} ms/loop");
            results.Add($"Sieve Size: {sieve.SieveSize}");
            results.Add($"Thread Count: {threads}");
            if (sieve.IsParallel)
            {
                results.Add($"Parallel Thread Count: {pthreads}");
            }
            results.Add($"Primes: {sieve.CountPrimes()}");
            results.Add($"Valid: {PrimeData.IsCountCorrect(sieve.SieveSize, sieve.CountPrimes())?.ToString() ?? "Unable to determine"}");
            if (sieve.ClearCount > 0)
            {
                results.Add($"Clear Count: {sieve.ClearCount}");
            }

            Console.WriteLine(results.Aggregate((a, b) => $"{a}, {b}"));

            // official syntax:
            // kinematics_<sieve_tag>;<pass_count>;<runtime>;<pthread_count>

            StringBuilder sb = new();

            sb.Append($"kinematics_{sieve.Name};");
            sb.Append($"{passes};");
            sb.AppendFormat("{0:G6};", watch.Elapsed.TotalSeconds);
            sb.Append($"{threads * pthreads};");
            sb.Append($"algorithm={(sieve.IsBaseAlgorithm ? "base" : "wheel")};");
            sb.Append($"faithful={(sieve.IsBaseAlgorithm ? "yes" : "no")};");
            sb.Append($"bits={sieve.BitsPerPrime}");
            sb.AppendLine();

            Console.WriteLine(sb.ToString());
        }

        private static Func<ISieveRunner>? GetSieveCreator(SieveProperty sieve, SettingsV2 settings)
        {
            return sieve switch
            {
                SievePropertyCombinations.Bit2 => () => new Bit2Sieve(settings.SieveSize),
                SievePropertyCombinations.Bit2While => () => new Bit2WhileSieve(settings.SieveSize),
                SievePropertyCombinations.Bit6 => () => new Bit6Sieve(settings.SieveSize),
                SievePropertyCombinations.Bit30 => () => new Bit30Sieve(settings.SieveSize),
                SievePropertyCombinations.Bool2 => () => new Bool2Sieve(settings.SieveSize),
                SievePropertyCombinations.Bool2While => () => new Bool2WhileSieve(settings.SieveSize),
                SievePropertyCombinations.Bool6 => () => new Bool6Sieve(settings.SieveSize),
                SievePropertyCombinations.Bool30 => () => new Bool30Sieve(settings.SieveSize),
                SievePropertyCombinations.IBool2 => () => new IBool2Sieve(settings.SieveSize),
                SievePropertyCombinations.IBool2While => () => new IBool2WhileSieve(settings.SieveSize),
                SievePropertyCombinations.IBool6 => () => new IBool6Sieve(settings.SieveSize),
                SievePropertyCombinations.IBool30 => () => new IBool30Sieve(settings.SieveSize),
                SievePropertyCombinations.PoolB2 => () => new PoolB2Sieve(settings.SieveSize),
                SievePropertyCombinations.PoolD2 => () => new PoolD2Sieve(settings.SieveSize),
                SievePropertyCombinations.PoolQ2 => () => new PoolQ2Sieve(settings.SieveSize),
                SievePropertyCombinations.PoolB6 => () => new PoolB6Sieve(settings.SieveSize),
                SievePropertyCombinations.PoolD6 => () => new PoolD6Sieve(settings.SieveSize),
                SievePropertyCombinations.PoolQ6 => () => new PoolQ6Sieve(settings.SieveSize),
                SievePropertyCombinations.PoolB30 => () => new PoolB30Sieve(settings.SieveSize),
                SievePropertyCombinations.PoolD30 => () => new PoolD30Sieve(settings.SieveSize),
                SievePropertyCombinations.PoolQ30 => () => new PoolQ30Sieve(settings.SieveSize),
                SievePropertyCombinations.PoolQ30M => () => new PoolQ30MSieve(settings.SieveSize),
                SievePropertyCombinations.RawB2 => () => new RawB2Sieve(settings.SieveSize),
                SievePropertyCombinations.RawD2 => () => new RawD2Sieve(settings.SieveSize),
                SievePropertyCombinations.RawQ2 => () => new RawQ2Sieve(settings.SieveSize),
                SievePropertyCombinations.RawB6 => () => new RawB6Sieve(settings.SieveSize),
                SievePropertyCombinations.RawD6 => () => new RawD6Sieve(settings.SieveSize),
                SievePropertyCombinations.RawQ6 => () => new RawQ6Sieve(settings.SieveSize),
                SievePropertyCombinations.RawB30 => () => new RawB30Sieve(settings.SieveSize),
                SievePropertyCombinations.RawD30 => () => new RawD30Sieve(settings.SieveSize),
                SievePropertyCombinations.RawQ30 => () => new RawQ30Sieve(settings.SieveSize),
                SievePropertyCombinations.RawQ30M => () => new RawQ30MSieve(settings.SieveSize),
                _ => null
            };
        }
    }
}
