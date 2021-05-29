using System;
using System.Collections.Generic;
using CommandLine;
using PrimeCSharp.Benchmarks;

namespace PrimeCSharp
{
    class Program
    {
        static void Main(string[] args)
        {
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
