using System.Collections.Generic;

namespace PrimeCSharp
{
    public interface ISieve
    {
        int SieveSize { get; }
        void Run();
        int CountPrimes();
        IEnumerable<int> GetFoundPrimes();

        bool IsParallel => false;

        public bool? IsCountValid()
        {
            return PrimeData.IsCountCorrect(SieveSize, CountPrimes());
        }

        int ClearCount => 0;
    }
}
