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
        int numItems = (int)((mNumBits + 31) / 32);
        mSieveBits = new uint32[numItems];
        Internal.MemSet(mSieveBits.CArray(), 0xff, sizeof(uint32) * numItems);
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
        for (uint64 bit = startBit; bit < mNumBits; bit += inc)
        {
            mSieveBits[(int)(bit >> 5)] &= ~(1 << (bit & 0x1f));
        }
    }

    public uint64 CountPrimes(bool showResults)
    {
        uint64 numPrimes = 0;
        if (mSieveSize >= 2)
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
            "rzuckerm;{};{};1;algorithm=base,faithful=yes,bits=1",
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
    public static void ShowHelp(StringView errorMessage="")
    {
        int statusCode = 0;
        if (errorMessage.Length > 0)
        {
            Console.WriteLine(errorMessage);
            Console.WriteLine();
            statusCode = 1;
        }

        Console.WriteLine("Options:\n");
        Console.WriteLine("--limit=<sieveSize>  Upper limit for calculating primes");
        Console.WriteLine("--time=<timeLimit>   Time limit in seconds");
        Console.WriteLine("--show               Print found prime numbers");
        Console.WriteLine("--help               Show help message");
        Environment.Exit(statusCode);
    }

    public static Result<T> ParseInt<T>(StringView str)
    where T : IParseable<T>
    {
        StringView trimmedStr = scope String(str);
        trimmedStr.Trim();
        return T.Parse(trimmedStr);
    }

    public static T ParseIntOrDie<T>(StringView str, StringView errorMessage)
    where T : IParseable<T>
    {
        T val = default(T);
        switch (ParseInt<T>(str))
        {
            case .Err:
                Console.WriteLine(errorMessage);
                Environment.Exit(1);
            case .Ok(out val):
        }

        return val;
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

            if (optionName == "--limit")
            {
                options.sieveSize = ParseIntOrDie<uint64>(optionValue, @"{arg}: Invalid sieve size");
            }
            else if (optionName == "--time")
            {
                options.timeLimit = ParseIntOrDie<uint32>(optionValue, @"{arg}: Invalid time limit");
            }
            else if (arg == "--show")
            {
                options.showResults = true;
            }
            else if (arg == "--help")
            {
                ShowHelp();
            }
            else
            {
                ShowHelp(@"Invalid option '{arg}'");
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
