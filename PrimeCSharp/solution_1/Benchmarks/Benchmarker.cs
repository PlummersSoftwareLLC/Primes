using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BenchmarkDotNet.Running;

namespace PrimeCSharp.Benchmarks
{
    class Benchmarker
    {
        public static void RunBenchmark(string? option)
        {
            _ = option?.ToLower() switch
            {
                "mod" => BenchmarkRunner.Run<BenchmarkMods>(),
                "ref" => BenchmarkRunner.Run<BenchmarkRef>(),
                "ofn" => BenchmarkRunner.Run<BenchmarkOfN>(),
                "par" => BenchmarkRunner.Run<BenchmarkPar>(),
                _ => BenchmarkRunner.Run<BenchmarkSieves>()
            };
        }
    }
}
