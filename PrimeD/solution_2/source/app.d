// Simple helper func used to align `value` into a power-of-two `Boundary`.
// The first set of parameters define template params.
// The second set defines runtime parameters.
T alignTo(alias Boundary, T)(T value)
{
    return (value + (Boundary * (value % Boundary > 0))) & ~(Boundary-1);
}
unittest
{
    // D has built-in unittests which can be run with `dub test`.
    assert(alignTo!16(0) == 0);
    assert(alignTo!16(16) == 16);
    assert(alignTo!16(8) == 16);
    assert(alignTo!16(31) == 32);
}

// Final can't be inherited from. This also means that the functions of a final class are non-virtual.
// The parenthesis indicates that this is a templated class.
final class Sieve(uint SieveSize)
if(SieveSize > 0) // We can attach constraints onto templated things that must succeed, otherwise the compiler will raise an error.
{
    // D allows nesting imports into _any_ scope, in order to avoid symbol pollution, and
    // it also allows D to write more "file-portable" code.
    import core.time : Duration;

    // You can apply qualifiers and attributes in a block, kind of like `public:` from C/C++, 
    // but less annoying.
    private
    {
        // Because SieveSize is a templated value, we know what it is at compile-time, so can stack allocate here.
        // Most D functions can be ran at compile-time. This is called Compile Time Function Execution (CTFE).
        // `alignTo` is being used in CTFE here.
        ubyte[alignTo!8(SieveSize) / 8] _bits = ubyte.max;
    }

    // We'll use this helper func at compile-time to ensure that the user passes
    // a valid value into the `SieveSize` template parameter for our class below.
    static bool isValidSieveSize(uint size)
    {
        import std.algorithm : canFind;
        return [10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000].canFind(size);
    }

    // This then allows us to perform our own compile-time checks with custom error messages!
    static assert(isValidSieveSize(SieveSize), "You have provided an invalid sieve size.");

    // You can of course also do the C/C++ style to avoid a level of indentation.
    // `@safe` is a built-in attribute that specifies "the compiler can guarentee(citation needed) this code is memory safe".
    @safe:

    // Members are public by default.
    void runSieve()
    {
        // We can also selectively specify which symbols from a package we want to import.
        // It's good practice to do so (less symbol bloat), but it's also super tedious.
        import std.algorithm : each;
        import std.range     : iota;
        import std.conv      : to;
        import std.math      : sqrt, round;

        // Something to note about the below code:
        //      We're using the `to` function from `std.conv` using member function syntax?
        //      That's because D has a concept called UFCS (Uniform Function Call Syntax)
        //      Most function calls written as `func(a, b, c)` can be written as `a.func(b, c)`.
        //
        // Also, in D we pass template parameters via `!(templateP1, templateP2)(runtimeP1, runtimeP2)`. 
        // If there's only one template parameter, then you can usually use the form `!templateP(runtimeP1, runtimeP2)`
        //
        // Also, also, if a function doesn't need any runtime parameters, you can just completely omit the parenthesis:
        //  `.to!int` and `.to!int()` are exactly the same.

        auto factor = 3;
        const q = sqrt(SieveSize.to!float).round.to!uint;

        while(factor < q)
        {
            // Semi-traditional style of a for each loop.
            foreach(i; factor..q) // every number from `factor` to `q`(exclusive)
            {
                if(this.getBit(i))
                {
                    factor = i;
                    break;
                }
            }

            // This is a more functional style of for each.
            // Note that we create and pass a delegate/lambda into a _template_ parameter.
            // This can allow D compilers to perform optimisations, including inlining.
            iota(factor * 3, SieveSize, factor * 2)
                .each!(num => this.clearBit(num));

            factor += 2;
        }
    }

    void printResults(string tag, string attribs, uint threadCount, bool showResults, Duration duration, uint passes) @trusted // `stderr` is unsafe apparently
    {
        import std.array  : Appender;
        import std.conv   : to;
        import std.stdio  : writeln, writefln, stderr;
        import std.format : format;

        // Similar to `StringBuilder` from C#
        Appender!(char[]) output;

        if(showResults)
            output.put("2, ");

        auto count = 1;
        foreach(num; 3..SieveSize)
        {
            if(this.getBit(num))
            {
                if(showResults)
                    output.put(format!"%s, "(num)); // We can pass the format specification as a template parameter
                                                    // which allows the function, *at compile time*, to check that we've
                                                    // given it enough parameters, and also correct value types in the right positions.
                count++;
            }
        }

        assert(count == this.countPrimes()); // Disabled within release builds.
        if(showResults)
            stderr.writeln(output.data);
        // Again, we can let it check at compile-time that everything's correct.
        // But also notice that we're just using `%s` for everything.
        // That's because D's `%s` will convert a value into it's most appropriate human-readable form.
        // It can even automatically, without any boilerplate, convert structs into a human readable string.
        // Anything other than `%s` is mostly used just for special formatting.
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

    // Since this function is only ever called after all data mutations are done, it seems appropriate to state that it's `pure`.
    private bool validateResults() pure
    {
        // We can embed structs directly into functions.
        // By marking it 'static' we're saying that it doesn't need access to the function context.
        static struct PrimeCountPair
        {
            uint primeLimit;
            uint primeCount;
        }

        // Dynamically generate code.
        // The foreach body is duplicated (unrolled) for every value inside the array.
        // D lets us use underscores to make numbers easier to read.
        static foreach(pair; [
                PrimeCountPair(10,              4),
                PrimeCountPair(100,             25),
                PrimeCountPair(1000,            168),
                PrimeCountPair(10_000,          1229),
                PrimeCountPair(100_000,         9592),
                PrimeCountPair(1_000_000,       78_498),
                PrimeCountPair(10_000_000,      664_579),
                PrimeCountPair(100_000_000,     5_761_455)
        ])
        {
            // Conditionally compile code.
            static if(SieveSize == pair.primeLimit)
                return this.countPrimes() == pair.primeCount;
        }
    }

    private bool getBit(uint index) @nogc nothrow
    {
        // Fairly standard bitty stuff.
        return (index % 2) == 0
            ? false
            : (this._bits[index / 8] & (1 << (index % 8))) != 0;
    }

    private void clearBit(uint index) @nogc nothrow
    {
        this._bits[index / 8] &= ~(1 << (index % 8));
    }

    private uint countPrimes() nothrow pure
    {
        import std.algorithm : map, sum;
        import std.range     : iota;

        // D has a concept called `ranges`, which are (usually) lightweight structs
        // which provide a .popFront(), .front(), and .empty() functions.
        //
        // They are used to produce a range of values in a lazily-evaluated manner, often without
        // any/reduced heap allocations.
        //
        // The standard library provides a bunch of ranges which can be composed together to form
        // pipelines.
        //
        // So essentially all we're doing is making a lazily evaluated, allocationless pipeline:
        //  Get every number in the range [0..size] exclusive.
        //  Map it to 1 if it is a prime, map it to 0 otherwise.
        //  Find the sum of all the mapped results.
        return iota(0, SieveSize)
                .map!(num => this.getBit(num) ? 1 : 0) // Ternary operator is just to make it more clear, not actually needed.
                .sum;
    }
}

void main()
{
    runSingleThreaded();
    runMultiThreaded();
}

void runSingleThreaded()
{
    import std.datetime.stopwatch : StopWatch, AutoStart;

    // `enum` here means `value that only exists at compile-time, and does not take up stack/memory space`
    enum PRIME_COUNT = 1_000_000;
    enum MAX_SECONDS = 5;

    auto passes = 1u;
    auto timer = StopWatch(AutoStart.yes);

    // One interesting usage of templates: Specifying a unit to act on.
    while(timer.peek.total!"seconds" < MAX_SECONDS)
    {
        scope sieve = new Sieve!PRIME_COUNT;
        sieve.runSieve();
        passes++;
    }

    const elapsedTime = timer.peek();
    
    auto s = new Sieve!PRIME_COUNT;
    s.runSieve();
    s.printResults("BradleyChatha", "algorithm=base,faithful=yes,bits=1", 1, false, elapsedTime, passes);
}

void runMultiThreaded()
{
    import core.atomic : atomicOp;
    import std.datetime.stopwatch : StopWatch, AutoStart;
    import std.range : iota;
    import std.parallelism : totalCPUs, parallel;

    enum PRIME_COUNT = 1_000_000;
    enum MAX_SECONDS = 5;

    shared passes = 1u; // `shared` is used to mark data that is used between multiple threads.
    auto timer = StopWatch(AutoStart.yes);

    while(timer.peek.total!"seconds" < MAX_SECONDS)
    {
        // .parallel makes the foreach body run on worker threads. It defaults to using as many
        // threads as there are CPUs (a.k.a totalCPUs).
        //
        // D makes multi-threaded processing painfully easy.
        foreach(i; iota(0, totalCPUs).parallel)
        {
            scope sieve = new Sieve!PRIME_COUNT;
            sieve.runSieve();
            atomicOp!"+="(passes, 1);
        }
    }

    const elapsedTime = timer.peek();
    
    auto s = new Sieve!PRIME_COUNT;
    s.runSieve();
    s.printResults(
        "BradleyChatha-Multi", 
        "algorithm=base,faithful=yes,bits=1", 
        totalCPUs, 
        false, 
        elapsedTime, 
        passes
    );
}
