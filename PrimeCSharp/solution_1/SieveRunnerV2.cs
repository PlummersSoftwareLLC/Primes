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

        /// <summary>
        /// Run all requested sieves based on the provided settings.
        /// </summary>
        /// <param name="settings">Collection of options used to determine which sieves to run, and parameters thereof.</param>
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

        /// <summary>
        /// Run a specific sieve using the provided parameters.
        /// </summary>
        /// <param name="sieve">The sieve to run, as determined by a combination of properties.</param>
        /// <param name="settings">The running parameters to use.</param>
        /// <exception cref="ArgumentException">If no sieve class matching the requested sieve can be found, an exception will be thrown.</exception>
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

        /// <summary>
        /// Run a provided sieve on a single thread, repeatedly, for as long as specified.
        /// </summary>
        /// <param name="createSieve">The function that creates a sieve.</param>
        /// <param name="watch">The time tracking watch.</param>
        /// <param name="millisecondsToRun">How long to run.</param>
        /// <param name="settings">Settings that may affect how the sieves are run.</param>
        /// <returns>Returns the last sieve, and how many times the sieve was run.</returns>
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

        /// <summary>
        /// Run a provided sieve on multiple threads, for as long as specified.
        /// </summary>
        /// <param name="createSieve">The function that creates a sieve.</param>
        /// <param name="watch">The time tracking watch.</param>
        /// <param name="millisecondsToRun">How long to run.</param>
        /// <param name="settings">Settings that may affect how the sieves are run.</param>
        /// <returns>Returns the first sieve, and how many times the sieve was run.</returns>
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

        /// <summary>
        /// A warmup function to allow a sieve to be run a few times to get the JIT familiar with the code.
        /// </summary>
        /// <param name="createSieve">The function to create the sieve we're running.</param>
        private static void Warmup(Func<ISieveRunner> createSieve)
        {
            for (int i = 0; i < WarmupCount; i++)
            {
                var sieve = createSieve();
                sieve.Run();
            }
        }

        /// <summary>
        /// Print the final output to the console.
        /// </summary>
        /// <param name="sieve">The sieve that was run.</param>
        /// <param name="passes">How many times it was run.</param>
        /// <param name="watch">The stopwatch used to time it.</param>
        /// <param name="settings">The settings that were in effect.</param>
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

            (int threads, int pThreads) = GetThreadCount(sieve, settings);

            Console.WriteLine(GetLongOutputDescription(sieve, passes, watch, threads, pThreads));

            Console.WriteLine(GetCompactOutputDescription(sieve, passes, watch, threads, pThreads));
        }

        /// <summary>
        /// Calculate the number of threads used when running a sieve.
        /// </summary>
        /// <param name="sieve">The sieve that was run.</param>
        /// <param name="settings">The run settings.</param>
        /// <returns>Returns a tuple of external threads used (each one running one sieve) and internal threads used (how many threads each sieve uses).</returns>
        private static (int thraeds, int pThreads) GetThreadCount(ISieveRunner sieve, SettingsV2 settings)
        {
            int threads = 1;
            if (settings.MultiThreaded)
            {
                threads = settings.ThreadCount == 0 ? Environment.ProcessorCount : settings.ThreadCount;
            }

            int pThreads = 1;
            if (sieve.IsParallel)
            {
                pThreads = settings.PThreadCount == 0 ? Environment.ProcessorCount : settings.PThreadCount;
            }

            return (threads, pThreads);
        }

        /// <summary>
        /// Get the long form output of the sieve run.
        /// </summary>
        private static string GetLongOutputDescription(ISieveRunner sieve, int passes, Stopwatch watch, int threads, int pThreads)
        {
            List<string> results = new();

            results.Add($"Passes: {passes}");
            results.Add($"Time: {watch.Elapsed.TotalSeconds:G6} s");
            results.Add($"{((double)watch.ElapsedMilliseconds / passes):F6} ms/loop");
            results.Add($"Sieve Size: {sieve.SieveSize}");
            results.Add($"Thread Count: {threads}");
            if (sieve.IsParallel)
            {
                results.Add($"Parallel Thread Count: {pThreads}");
            }
            results.Add($"Primes: {sieve.CountPrimes()}");
            results.Add($"Valid: {PrimeData.IsCountCorrect(sieve.SieveSize, sieve.CountPrimes())?.ToString() ?? "Unable to determine"}");
            if (sieve.ClearCount > 0)
            {
                results.Add($"Clear Count: {sieve.ClearCount}");
            }

            return results.Aggregate((a, b) => $"{a}, {b}");
        }

        /// <summary>
        /// Get the compact output of the sieve run.
        /// </summary>
        private static string GetCompactOutputDescription(ISieveRunner sieve, int passes, Stopwatch watch, int threads, int pThreads)
        {
            // official syntax:
            // <label>;<pass_count>;<runtime>;<num_threads>;<tags (if any)>

            StringBuilder sb = new();

            // Fields:
            sb.Append($"kinematics_{sieve.Name};");
            sb.Append($"{passes};");
            sb.AppendFormat("{0:G6};", watch.Elapsed.TotalSeconds);
            sb.Append($"{threads * pThreads};");

            // Tags:
            sb.Append($"algorithm={sieve.AlgorithmType},");
            sb.Append($"faithful={(sieve.IsFaithful ? "yes" : "no")},");
            sb.Append($"bits={sieve.BitsPerPrime}");
            sb.AppendLine();

            return sb.ToString();
        }

        /// <summary>
        /// Function to translate sieve property presets into functions to create a sieve object.
        /// </summary>
        /// <param name="sieve">The sieve requested.</param>
        /// <param name="settings">The settings to be used during the sieve's run.</param>
        /// <returns>Returns a function to create an instance of the requested sieve.</returns>
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
                SievePropertyCombinations.InvBool2 => () => new InvBool2Sieve(settings.SieveSize),
                SievePropertyCombinations.InvBool2While => () => new InvBool2WhileSieve(settings.SieveSize),
                SievePropertyCombinations.InvBool6 => () => new InvBool6Sieve(settings.SieveSize),
                SievePropertyCombinations.InvBool30 => () => new InvBool30Sieve(settings.SieveSize),
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
                SievePropertyCombinations.PoolQ2M => () => new PoolQ2MSieve(settings.SieveSize),
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
