using System;
using System.Collections.Generic;

namespace PrimeCSharp
{
    public class PrimeData
    {
        public static readonly IReadOnlyDictionary<int, int> PrimeCounts = new Dictionary<int, int>
            {
                { 10 , 4 },                 // Historical data for validating our results - the number of primes
                { 100 , 25 },               // to be found under some limit, such as 168 primes under 1000
                { 200 , 46 },
                { 300 , 62 },
                { 400 , 78 },
                { 500 , 95 },
                { 600 , 109 },
                { 700 , 125 },
                { 800 , 139 },
                { 900 , 154 },
                { 1000 , 168 },
                { 10000 , 1229 },
                { 100000 , 9592 },
                { 1000000 , 78498 },
                { 10000000 , 664579 },
                { 100000000 , 5761455 }
            };

        public static bool? IsCountCorrect(int limit, int found)
        {
            if (PrimeCounts.TryGetValue(limit, out int primeCount))
            {
                return found == primeCount;
            }

            return null;
        }
    }
}
