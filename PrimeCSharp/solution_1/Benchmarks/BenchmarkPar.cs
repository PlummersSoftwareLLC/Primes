using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BenchmarkDotNet.Attributes;
using PrimeCSharp.V1Sieves;

namespace PrimeCSharp.Benchmarks
{
    public class BenchmarkPar
    {
        [Params(100_000_000)]
        public int SieveSize { get; set; }

        [Params(32)]
        public int PThreads { get; set; }


        [Benchmark(Baseline = true)]
        public void ArrayPool2of6()
        {
            new PrimeSieveArrayPool2Of6(SieveSize).Run();
        }

        [Benchmark]
        public void ArrayPool2of6Parallel()
        {
            new PrimeSieveArrayPool6Par(SieveSize, PThreads).Run();
        }
    }
}
