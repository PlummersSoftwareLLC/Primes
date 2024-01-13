using System;
using System.Diagnostics;

namespace Primes;

class PrimeSieve
{
    public readonly uint64 mSieveSize;
    public readonly uint64 mNumBits;
    private uint32[] mSieveBits ~ delete _;

    public this(uint64 sieveSize)
    {
        mSieveSize = sieveSize;
        mNumBits = (sieveSize - 1) / 2;
        int numBytes = (int)((mNumBits + 31) / 32);
        mSieveBits = new uint32[numBytes];
        Internal.MemSet(mSieveBits.CArray(), 0xff, sizeof(uint32) * numBytes);
    }

    [Optimize]
    [DisableChecks]
    public void Sieve()
    {
        uint64 q = (uint64)((-3 + Math.Sqrt(3 + 2 * mNumBits)) / 2);
        for (uint64 bit = 0; bit <= q; bit++)
        {
            if (GetBit(bit))
            {
                ClearBits(2 * (bit + 1) * (bit + 2) - 1, 2 * bit + 3);
            }
        }
    }

    [Optimize]
    [DisableChecks]
    [Inline]
    private bool GetBit(uint64 bit)
    {
        return mSieveBits[(int)(bit >> 5)] & (1 << (bit & 0x1f)) != 0;
    }

    [Optimize]
    [DisableChecks]
    [Inline]
    private void ClearBits(uint64 startBit, uint64 inc)
    {
        int bit = (int)(startBit & 0x1f);
        int byte = (int)(startBit >> 5);
        int bitInc = (int)(inc & 0x1f);
        int byteInc = (int)(inc >> 5);
        for (uint64 currBit = startBit; currBit < mNumBits; currBit += inc)
        {
            mSieveBits[byte] &= ~(1 << bit);
            bit += bitInc;
            byte += byteInc;
            if (bit >= 32) {
                bit -= 32;
                byte++;
            }
        }
    }

    public uint64 CountPrimes(bool showResults)
    {
        uint64 numPrimes = 0;
        if (mNumBits > 0)
        {
            numPrimes++;
            if (showResults)
            {
                Console.Write("2, ");
            }
        }

        for (uint64 bit = 0; bit < mNumBits; bit++)
        {
            if (GetBit(bit))
            {
                numPrimes++;
                if (showResults)
                {
                    Console.Write($"{2 * bit + 3}, ");
                }
            }
        }

        Console.WriteLine();

        return numPrimes;
    }

    public uint64 GetExpectedPrimesCount()
    {
        switch (mSieveSize)
        {
            case 10: return 4;
            case 100: return 25;
            case 1000: return 168;
            case 10000: return 1229;
            case 100000: return 9592;
            case 1000000: return 78498;
            case 10000000: return 664579;
            case 100000000: return 5761455;
            case 1000000000: return 50847534;
            case 10000000000: return 455052511;
        }

        return 0;
    }

    public bool ValidatePrimesCount(uint64 primesCount)
    {
        return primesCount == GetExpectedPrimesCount();
    }

    public void ShowResults(bool showResults, uint64 passes, int64 elapsedTimeMsec)
    {
        uint64 primeCount = CountPrimes(showResults);
        bool valid = ValidatePrimesCount(primeCount);
        Console.WriteLine(
            "Passes: {}, Time: {}ms, Avg: {}ms, Limit: {}, Count: {}, Valid: {}",
            passes,
            elapsedTimeMsec,
            (double)elapsedTimeMsec / passes,
            mSieveSize,
            primeCount,
            valid
        );
        Console.WriteLine(
            "rzuckerm;{};{};1;algorithm=base,faithful=yes",
            passes,
            (double)elapsedTimeMsec / 1000.0
        );
    }
}

struct PrimeOptions
{
    public uint64 sieveSize = 1'000'000;
    public uint32 timeLimit = 5;
    public bool showResults = false;
}

class Program
{
    public static void ShowHelp()
    {
        Console.WriteLine("Options:\n");
        Console.WriteLine("--limit=<sieveSize>  Upper limit for calculating primes");
        Console.WriteLine("--time=<timeLimit>   Time limit in seconds");
        Console.WriteLine("--show               Print found prime numbers");
        Console.WriteLine("--help               Show help message");
        Environment.Exit(1);
    }

    public static Result<T> ParseInt<T>(StringView str)
    where T : IParseable<T>
    {
        StringView trimmedStr = scope String(str);
        trimmedStr.Trim();

        // For some reason T.Parse does not treat a sign without a number is not an error.
        if (trimmedStr == "-" || trimmedStr == "+")
        {
            return .Err;
        }

        return T.Parse(trimmedStr);
    }

    public static T ParseIntOrDie<T>(StringView str, StringView errorMessage)
    where T : IParseable<T>
    {
        switch (ParseInt<T>(str))
        {
            case .Err:
                Console.WriteLine(errorMessage);
                Environment.Exit(1);
                return default(T); // Get compiler to stop complaining about no return value
            case .Ok(let val):
                return val;
        }
    }

    public static PrimeOptions ParseArgs(String[] args)
    {
        PrimeOptions options = PrimeOptions();
        for (String arg in args)
        {
            int index = arg.IndexOf('=');
            StringView optionName = "";
            StringView optionValue = "";
            if (index < 0)
            {
                optionName = arg;
            }
            else
            {
                optionName = arg.Substring(0, index);
                optionValue = arg.Substring(index + 1);
            }

            String errorMessage = scope String(arg);
            if (optionName == "--limit")
            {
                errorMessage.Append(": Invalid sieve size");
                options.sieveSize = ParseIntOrDie<uint64>(optionValue, errorMessage);
            }
            else if (optionName == "--time")
            {
                errorMessage.Append(": Invalid time limit");
                options.timeLimit = ParseIntOrDie<uint32>(optionValue, errorMessage);
            }
            else if (optionName == "--show")
            {
                options.showResults = true;
            }
            else if (optionName == "--help")
            {
                ShowHelp();
            }
            else
            {
                Console.WriteLine($"Invalid option '{arg}'\n");
                ShowHelp();
            }
        }

        return options;
    }

    public static void TimedRunSieve(PrimeOptions options)
    {
        uint64 passes = 0;
        int64 timeLimitMsec = (int64)options.timeLimit * 1000;
        Stopwatch watch = scope Stopwatch(true);
        while (true)
        {
            passes++;
            PrimeSieve sieve = scope PrimeSieve(options.sieveSize);
            sieve.Sieve();
            if (watch.ElapsedMilliseconds >= timeLimitMsec)
            {
                watch.Stop();
                sieve.ShowResults(options.showResults, passes, watch.ElapsedMilliseconds);
                break;
            }
        }
    }

    public static int Main(String[] args)
    {
        PrimeOptions options = ParseArgs(args);
        TimedRunSieve(options);
        return 0;
    }
}
