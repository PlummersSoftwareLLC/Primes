using System;
using System.Collections.Generic;
using System.Linq;

namespace PrimeCSharp.SieveRunners
{
    public interface ISieveRunner
    {
        // Identity
        public string Name { get; }
        public string Description { get; }

        // Runner details (default values)
        bool IsParallel => false;
        bool IsBaseAlgorithm => false;
        int BitsPerPrime => 1;

        // Runtime details
        public int SieveSize { get; }

        // Functionality
        void Run();
        IEnumerable<int> GetFoundPrimes();
        int CountPrimes() => GetFoundPrimes().Count();
        public bool? IsCountValid() => PrimeData.IsCountCorrect(SieveSize, CountPrimes());

        // Debug
        int ClearCount { get; set; }
    }
}
