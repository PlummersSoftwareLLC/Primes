using System;
using BenchmarkDotNet.Attributes;

namespace PrimeCSharp.Benchmarks
{
    public class BenchmarkMods
    {
        [Params(1000000)]
        public int SieveSize { get; set; }


        [Benchmark(Baseline = true)]
        public void StandardSieveMod()
        {
            new PrimeSieveMod(SieveSize).Run();
        }

        [Benchmark]
        public void StandardSieveAnd()
        {
            new PrimeSieveAnd(SieveSize).Run();
        }

        [Benchmark]
        public void StandardSieveUnMod()
        {
            new PrimeSieveUnMod(SieveSize).Run();
        }

        [Benchmark]
        public void StandardSieveUnAnd()
        {
            new PrimeSieveUnAnd(SieveSize).Run();
        }
    }
}
