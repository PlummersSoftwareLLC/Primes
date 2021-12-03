using BenchmarkDotNet.Running;

namespace PrimeCSharp.Benchmarks
{
    class Benchmarker
    {
        public static void RunBenchmark(string version, string? option)
        {
            _ = option?.ToLower() switch
            {
                "mod" => BenchmarkRunner.Run<BenchmarkMods>(),
                "ref" => BenchmarkRunner.Run<BenchmarkRef>(),
                "ofn" => BenchmarkRunner.Run<BenchmarkOfN>(),
                "par" => BenchmarkRunner.Run<BenchmarkPar>(),
                _ => version switch
                {
                    "v1" => BenchmarkRunner.Run<BenchmarkV1Sieves>(),
                    _ => BenchmarkRunner.Run<BenchmarkV2Sieves>()
                }
            };
        }
    }
}
