using System;
using System.Collections.Generic;
using System.Globalization;
using CommandLine;
using PrimeCSharp.Benchmarks;
using PrimeCSharp.Config;

namespace PrimeCSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            CultureInfo.CurrentCulture = new CultureInfo("en-US", false);

            if (args[0] == "v2")
            {
                var v2args = args[1..];

                var result2 = Parser.Default.ParseArguments<SettingsV2>(v2args)
                    .WithParsed(options => Run(options))
                    .WithNotParsed(errors => HandleErrors(errors)); // errors is a sequence of type IEnumerable<Error>

                return;
            }

            var result = Parser.Default.ParseArguments<RunSettings>(args)
                .WithParsed(options => Run(options))
                .WithNotParsed(errors => HandleErrors(errors)); // errors is a sequence of type IEnumerable<Error>
        }

        private static void Run(RunSettings options)
        {
            if (options.Benchmark)
            {
                Benchmarker.RunBenchmark(options.Bench);
            }
            else if (options.RunAllSieves)
            {
                SieveRunner.RunAllSieves(options);
            }
            else
            {
                SieveRunner.RunSieve(options);
            }
        }

        private static void Run(SettingsV2 options)
        {
            if (options.Benchmark)
                Benchmarker.RunBenchmark(options.Bench);
            else
                SieveRunnerV2.Run(options);
        }

        private static void HandleErrors(IEnumerable<Error> errors)
        {
            foreach (var error in errors)
            {
                if (error is HelpRequestedError ||
                    error is VersionRequestedError)
                    continue;

                if (error.StopsProcessing)
                {
                    Console.WriteLine(error.ToString());
                }
            }
        }
    }
}
