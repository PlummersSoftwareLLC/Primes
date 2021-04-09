using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.Contracts;

prime_sieve.Init();

prime_sieve sieve = null;
double elapsed;
var passes = 0;
var tStart = DateTime.UtcNow;

do
{
    sieve = new prime_sieve();
    sieve.runSieve();
    passes++;

    elapsed = (DateTime.UtcNow - tStart).TotalSeconds;
} while (elapsed < prime_sieve.DURATION);

sieve?.printResults(elapsed, passes);

class prime_sieve
{
    public const int SIEVE_SIZE = 1000000;
    public const int DURATION = 10;
    public static int BitArraySize;
    public static int SieveSizeSqrt;

    public static void Init()
    {
        BitArraySize = (int) ((SIEVE_SIZE + 1) / 2);
        SieveSizeSqrt = (int) Math.Sqrt(SIEVE_SIZE);
    }

    private readonly BitArray bitArray;
    private readonly Dictionary<int, int> myDict = new Dictionary<int, int> 
    { 
        { 10 , 1 },                 // Historical data for validating our results - the number of primes
        { 100 , 25 },               // to be found under some limit, such as 168 primes under 1000
        { 1000 , 168 },
        { 10000 , 1229 },
        { 100000 , 9592 },
        { 1000000 , 78498 },
        { 10000000 , 664579 },
        { 100000000 , 5761455 } 
    };

    public prime_sieve() 
    {
        bitArray = new BitArray(BitArraySize, true);
    }

    [Pure]
    public int countPrimes()
    {
        var count = 0;
        for (var i = 0; i < this.bitArray.Count; i++)
        {
            if (bitArray[i])
            {
                count++;
            }
        }
        return count;
    }

    [Pure]
    public bool validateResults()
    {
        if (myDict.ContainsKey(SIEVE_SIZE))
        {
            return this.myDict[SIEVE_SIZE] == this.countPrimes();
        }
        return false;
    }

    [Pure]
    private bool GetBit(int index)
    {
        if ((index & 1) == 0)
        {
            return false;
        }
        return bitArray[index / 2];
    }

    private void ClearBit(int index)
    {
#if DEBUG
        if ((index & 1) == 0)
        {
            throw new Exception("You are setting even bits, which is sub-optimal.");
            return;
        }
#endif
        bitArray[index / 2] = false;      
    }

    // primeSieve
    // 
    // Calculate the primes up to the specified limit

    public void runSieve()
    {
        var factor = 3;

        while (factor < SieveSizeSqrt)
        {
            for (var num = factor; num <= SIEVE_SIZE; num++)
            {
                if (GetBit(num))
                {
                    factor = num;
                    break;
                }
            }

            // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
            // We can then step by factor * 2 because every second one is going to be even by definition

            for (var num = factor * 3; num <= SIEVE_SIZE; num += factor * 2)
            {
                ClearBit(num);
            }

            factor += 2;
        }
    }

    public void printResults(double duration, int passes, bool showResults = false)
    {
        if (showResults)
            Console.Write("2, ");

        var count = 1;
        for (var num = 3; num <= SIEVE_SIZE; num++)
        {
            if (GetBit(num))
            {
                if (showResults)
                    Console.Write($"{num}, ");
                count++;
            }
        }
        if (showResults)
            Console.WriteLine("");
        Console.WriteLine($"Passes: {passes}, Time: {duration:n8}, Avg: {(duration / passes):n8}, Limit: {SIEVE_SIZE}, Count: {count}, Valid: {validateResults()}");
    }
}
