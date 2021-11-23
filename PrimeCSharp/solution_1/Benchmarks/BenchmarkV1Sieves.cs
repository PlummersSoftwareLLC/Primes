using System.Collections.Generic;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using PrimeCSharp.V1Sieves;

namespace PrimeCSharp.Benchmarks
{
    [MemoryDiagnoser]
    public class BenchmarkV1Sieves
    {
        [Params(1000000)]
        public int SieveSize { get; set; }


        [Benchmark]
        public void Original()
        {
            new PrimeSieve(SieveSize).Run();
        }

        [Benchmark(Baseline = true)]
        public void Standard()
        {
            new PrimeSieve(SieveSize).Run();
        }

        [Benchmark]
        public void Bool()
        {
            new PrimeSieveBool(SieveSize).Run();
        }

        [Benchmark]
        public void InvertedBool()
        {
            new PrimeSieveInvBool(SieveSize).Run();
        }

        [Benchmark]
        public void DirectInvertedBool()
        {
            new PrimeSieveInvBoolB(SieveSize).Run();
        }

        [Benchmark]
        public void RawBits()
        {
            new PrimeSieveRawBits(SieveSize).Run();
        }

        [Benchmark]
        public void ArrayPoolClass()
        {
            new PrimeSieveArrayPool(SieveSize).Run();
        }

        [Benchmark]
        public void ArrayPool2Of6()
        {
            new PrimeSieveArrayPool2Of6(SieveSize).Run();
        }

        [Benchmark]
        public void ArrayPool6Par()
        {
            new PrimeSieveArrayPool6Par(SieveSize, 3).Run();
        }

        [Benchmark]
        public void ArrayPool8of30()
        {
            new PrimeSieveArrayPool8of30(SieveSize).Run();
        }

        [Benchmark]
        public void ArrayPool8of30M()
        {
            new PrimeSieveArrayPool8of30M(SieveSize).Run();
        }
    }
}
