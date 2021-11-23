using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BenchmarkDotNet.Attributes;
using PrimeCSharp.V1Sieves;

namespace PrimeCSharp.Benchmarks
{
    public class BenchmarkRef
    {
        [Params(1000000)]
        public int SieveSize { get; set; }


        [Benchmark(Baseline = true)]
        public void StandardSieveMod()
        {
            new PrimeSieveMod(SieveSize).Run();
        }

        [Benchmark]
        public void ArrayPoolRef()
        {
            new PrimeSieveArrayPoolRef(SieveSize).Run();
        }

        [Benchmark]
        public void ArrayPoolClass()
        {
            new PrimeSieveArrayPool(SieveSize).Run();
        }
    }
}
