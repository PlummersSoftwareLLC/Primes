using BenchmarkDotNet.Attributes;
using PrimeCSharp.V2Sieves;

namespace PrimeCSharp.Benchmarks
{
    [MemoryDiagnoser]
    public class BenchmarkV2Sieves
    {
        [Params(1000000)]
        public int SieveSize { get; set; }


        [Benchmark(Baseline = true)]
        public void Bit2()
        {
            new Bit2Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void Bit2While()
        {
            new Bit2WhileSieve(SieveSize).Run();
        }

        [Benchmark]
        public void Bit6()
        {
            new Bit6Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void Bit30()
        {
            new Bit30Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void Bool2()
        {
            new Bool2Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void Bool6()
        {
            new Bool6Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void Bool30()
        {
            new Bool30Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void IBool2()
        {
            new InvBool2Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void IBool6()
        {
            new InvBool6Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void IBool30()
        {
            new InvBool30Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void PoolB2()
        {
            new PoolB2Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void PoolB6()
        {
            new PoolB6Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void PoolB30()
        {
            new PoolB30Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void PoolD2()
        {
            new PoolD2Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void PoolD6()
        {
            new PoolD6Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void PoolD30()
        {
            new PoolD30Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void PoolQ2()
        {
            new PoolQ2Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void PoolQ6()
        {
            new PoolQ6Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void PoolQ30()
        {
            new PoolQ30Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void PoolQ30M()
        {
            new PoolQ30MSieve(SieveSize).Run();
        }
        
        [Benchmark]
        public void RawB2()
        {
            new RawB2Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void RawB6()
        {
            new RawB6Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void RawB30()
        {
            new RawB30Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void RawD2()
        {
            new RawD2Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void RawD6()
        {
            new RawD6Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void RawD30()
        {
            new RawD30Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void RawQ2()
        {
            new RawQ2Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void RawQ6()
        {
            new RawQ6Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void RawQ30()
        {
            new RawQ30Sieve(SieveSize).Run();
        }

        [Benchmark]
        public void RawQ30M()
        {
            new RawQ30MSieve(SieveSize).Run();
        }
    }
}
