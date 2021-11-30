using System.Collections.Generic;

namespace PrimeCSharp
{
    public interface ISieve
    {
        string QuickName { get; }
        string Name { get; }

        int SieveSize { get; }
        void Run();
        int CountPrimes();
        IEnumerable<int> GetFoundPrimes();

        bool IsParallel => false;
        string AlgorithmType => "other";
        int? BitsPerPrime => null;

        public bool? IsCountValid()
        {
            return PrimeData.IsCountCorrect(SieveSize, CountPrimes());
        }

        int ClearCount => 0;
    }
}
