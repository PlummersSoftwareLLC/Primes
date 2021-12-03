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

            if (args.Length > 0 && args[0] == "v1")
            {
                var v1args = args[1..];

                var result = Parser.Default.ParseArguments<SettingsV1>(v1args)
                    .WithParsed(options => Run(options))
                    .WithNotParsed(errors => HandleErrors(errors)); // errors is a sequence of type IEnumerable<Error>

                return;
            }

            var result2 = Parser.Default.ParseArguments<SettingsV2>(args)
                .WithParsed(options => Run(options))
                .WithNotParsed(errors => HandleErrors(errors)); // errors is a sequence of type IEnumerable<Error>

        }

        private static void Run(SettingsV1 options)
        {
            if (options.Benchmark)
            {
                Benchmarker.RunBenchmark("v1", options.Bench);
            }
            else if (options.RunAllSieves)
            {
                SieveRunnerV1.RunAllSieves(options);
            }
            else
            {
                SieveRunnerV1.RunSieve(options);
            }
        }

        private static void Run(SettingsV2 options)
        {
            if (options.Benchmark)
                Benchmarker.RunBenchmark("v2", options.Bench);
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
