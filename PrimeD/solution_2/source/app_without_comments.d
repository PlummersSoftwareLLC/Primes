T alignTo(alias Boundary, T)(T value)
{
    return (value + (Boundary * (value % Boundary > 0))) & ~(Boundary-1);
}
unittest
{
    assert(alignTo!16(0) == 0);
    assert(alignTo!16(16) == 16);
    assert(alignTo!16(8) == 16);
    assert(alignTo!16(31) == 32);
}

final class SieveCT(uint SieveSize)
if(SieveSize > 0) // We can attach constraints onto templated things that must succeed, otherwise the compiler will raise an error.
{
    mixin CommonSieveFunctions;

    private
    {
        ubyte[alignTo!8(SieveSize) / 8] _bits = ubyte.max;
    }

    static bool isValidSieveSize(uint size)
    {
        import std.algorithm : canFind;
        return [10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000].canFind(size);
    }

    static assert(isValidSieveSize(SieveSize), "You have provided an invalid sieve size.");

    private bool validateResults() nothrow pure
    {
        static struct PrimeCountPair
        {
            uint primeLimit;
            uint primeCount;
        }

        static foreach(pair; [
                PrimeCountPair(10,          4),
                PrimeCountPair(100,         25),
                PrimeCountPair(1000,        168),
                PrimeCountPair(10_000,      1229),
                PrimeCountPair(100_000,     9592),
                PrimeCountPair(1_000_000,   78_498),
                PrimeCountPair(10_000_000,  664_579),
                PrimeCountPair(100_000_000, 5_761_455)
        ])
        {
            static if(SieveSize == pair.primeLimit)
                return this.countPrimes() == pair.primeCount;
        }
    }
}

mixin template CommonSieveFunctions()
{   
    import core.time : Duration;

    @safe:

    void runSieve()
    {
        import std.algorithm : each;
        import std.conv      : to;
        import std.math      : sqrt, round;
        import std.range     : iota;


        auto factor = 3;
        const q = sqrt(SieveSize.to!float).round.to!uint;

        while(factor < q)
        {
            foreach(i; iota(factor, q, 2)) // every number from `factor` to `q`(exclusive)
            {
                if(this.getBit(i))
                {
                    factor = i;
                    break;
                }
            }

            iota(factor * 3, SieveSize, factor * 2)
                .each!(num => this.clearBit(num));

            factor += 2;
        }
    }

    void printResults(
        string tag,
        string attribs,
        uint threadCount, 
        bool showResults,
        Duration duration,
        uint passes
    ) @trusted // `stderr` is unsafe apparently
    {
        import std.array  : Appender;
        import std.conv   : to;
        import std.stdio  : writeln, writefln, stderr;
        import std.format : format;
        import std.range  : iota;

        Appender!(char[]) output;

        if(showResults)
            output.put("2, ");

        auto count = 1;
        foreach(num; iota(3, SieveSize, 2))
        {
            if(this.getBit(num))
            {
                if(showResults)
                    output.put(format!"%s, "(num)); // We can pass the format specification as a template parameter
                count++;
            }
        }

        assert(count == this.countPrimes()); // Disabled within release builds.
        if(showResults)
            stderr.writeln(output.data);
        stderr.writefln!"Passes: %s, Time: %s, Avg: %s, Limit: %s, Count: %s, Valid: %s"
        (
            passes,
            duration,
            duration / passes,
            SieveSize,
            count,
            this.validateResults()
        );

        writefln!"%s;%s;%s;%s;%s"
        (
            tag,
            passes,
            duration.total!"nsecs".to!double / 1e+9, // Duration works in `long`, but we want a `double`, so this is just a conversion.
            threadCount,
            attribs
        );
    }

    private bool getBit(uint index) @nogc nothrow
    {
        assert(index % 2 == 1, "Index is even?");
        return (this._bits[index / 8] & (1 << (index % 8))) != 0;
    }

    private void clearBit(uint index) @nogc nothrow
    {
        this._bits[index / 8] &= ~(1 << (index % 8));
    }

    private uint countPrimes() nothrow
    {
        import std.algorithm : map, sum;
        import std.range     : iota;

        return iota(3, SieveSize, 2)
                .map!(num => this.getBit(num) ? 1 : 0) // Ternary operator is just to make it more clear, not actually needed.
                .sum + 1; // + 1 is to account for '2' being a special case.
    }
}

final class SieveRT
{
    mixin CommonSieveFunctions;

    private ubyte[] _bits;
    private uint _sieveSize;

    private alias SieveSize = _sieveSize;

    this(uint sieveSize)
    {
        this._bits.length = alignTo!8(sieveSize) / 8;
        this._bits[] = ubyte.max;
        this._sieveSize = sieveSize;
    }

    static struct PrimePair
    {
        uint sieveSize;
        uint primeCount;
    }

    @PrimePair(10_000, 1229)
    @PrimePair(1_000,  168)
    @(PrimePair(100, 25), PrimePair(10, 4)) // alternate syntax.
    private bool validateResults()
    {
        import std.algorithm : filter, map;
        import std.traits    : getUDAs;

        uint[uint] primeList = 
        [
            100_000_000 : 5_761_455,
            10_000_000  : 664_579,
            1_000_000   : 78_498,
            100_000     : 9592
        ];

        enum Pairs = getUDAs!(validateResults, PrimePair);
        static foreach(primePair; Pairs)
            primeList[primePair.sieveSize] = primePair.primeCount;

        const primes = this.countPrimes();
        if(this._sieveSize in primeList)
            return primeList[this._sieveSize] == primes;
        else
            return false; // TODO: Figure out how to determine validity for any arbitrary sieve size.
    }
}

enum PRIME_COUNT = 1_000_000;
enum MAX_SECONDS = 5;

import std.typecons : Flag, Yes, No;

alias IsFaithful = Flag!"faithful";

void main()
{
    runSingleThreaded!(SieveCT!PRIME_COUNT)(IsFaithful.no);
    runMultiThreaded!(SieveCT!PRIME_COUNT)(No.faithful); // Same thing as IsFaithful.no
    runSingleThreaded!SieveRT(IsFaithful.yes);
    runMultiThreaded!SieveRT(Yes.faithful);
}

void runSingleThreaded(alias SieveType)(IsFaithful faithful)
{
    import std.conv               : to;
    import std.datetime.stopwatch : StopWatch, AutoStart;

    auto passes = 1u;
    auto timer = StopWatch(AutoStart.yes);

    while(timer.peek.total!"seconds" < MAX_SECONDS)
    {

        static if(is(SieveType == SieveRT))
            scope sieve = new SieveType(PRIME_COUNT);
        else
            scope sieve = new SieveType();

        sieve.runSieve();
        passes++;
    }

    const elapsedTime    = timer.peek();
    const SieveClassName = __traits(identifier, SieveType); // __traits lets us learn a *lot* of useful info from the compiler, such as symbol names.

    static if(is(SieveType == SieveCT!Param1, uint Param1))
        auto s = new SieveType();
    else
        auto s = new SieveType(PRIME_COUNT);

    s.runSieve();
    s.printResults(
        "BradleyChatha-Single-"~SieveClassName, 
        "algorithm=base,bits=1,faithful="~(faithful.to!string), // Flag.to!string -> "yes" or "no". 
        1, 
        false, 
        elapsedTime, 
        passes
    );
}

void runMultiThreaded(alias SieveType)(IsFaithful faithful)
{
    import core.atomic            : atomicOp;
    import std.conv               : to;
    import std.datetime.stopwatch : StopWatch, AutoStart;
    import std.range              : iota;
    import std.parallelism        : totalCPUs, parallel;

    shared passes = 1u; // `shared` is used to mark data that is used between multiple threads.
    auto timer = StopWatch(AutoStart.yes);

    while(timer.peek.total!"seconds" < MAX_SECONDS)
    {
        foreach(i; iota(0, totalCPUs).parallel)
        {
            static if(__traits(compiles, new SieveType(PRIME_COUNT)))
                scope sieve = new SieveType(PRIME_COUNT);
            else
                scope sieve = new SieveType();
            sieve.runSieve();
            atomicOp!"+="(passes, 1);
        }
    }

    const elapsedTime = timer.peek();
    
    import std.traits : isInstanceOf;
    static if(isInstanceOf!(SieveCT, SieveType))
        auto s = new SieveType();
    else
        auto s = new SieveType(PRIME_COUNT);

    s.runSieve();
    s.printResults(
        "BradleyChatha-Multi-"~__traits(identifier, SieveType), 
        "algorithm=base,bits=1,faithful="~(faithful.to!string), 
        totalCPUs, 
        false, 
        elapsedTime, 
        passes
    );
}

