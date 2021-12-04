using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BenchmarkDotNet.Attributes;
using PrimeCSharp.V1Sieves;

namespace PrimeCSharp.Benchmarks
{
    public class BenchmarkOfN
    {
        [Params(1000000)]
        public int SieveSize { get; set; }


        [Benchmark]
        public void ArrayPoolClass()
        {
            new PrimeSieveArrayPool(SieveSize).Run();
        }

        [Benchmark]
        public void ArrayPoolClassOf6()
        {
            new PrimeSieveArrayPool2Of6(SieveSize).Run();
        }
    }
}
