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
final class Sieve(uint _sieveSize)
{
    // D allows nesting imports into _any_ scope, in order to avoid symbol pollution, and
    // it also allows D to write more "file-portable" code.
    import core.time : Duration;

    // You can apply qualifiers and attributes in a block, kind of like `public:` from C/C++, 
    // but less annoying.
    private
    {
        // Similar to C#'s `readonly`, we can initialise otherwise-const data inside of the ctor.
        //
        // `uint[uint]` is an associative array in D. It has the form `VALUE_TYPE[KEY_TYPE]`.
        //
        // Because _sieveSize is a templated value, we know what it is at compile-time, so can stack allocate here.
        ubyte[_sieveSize] _bits = ubyte.max;
        const uint[uint]  _historicalData;
    }

    // You can of course also do the C/C++ style to avoid a level of indentation.
    // `@safe` is a built-in attribute that specifies "the compiler can guarentee(citation needed) this code is memory safe".
    @safe:

    // This is a constructor in D.
    // `nothrow` of course means that this function doesn't throw exceptions, which allows the compiler
    // to remove any exception handling code it'd otherwise generate.
    this() nothrow
    {
        // D allows for underscores within a number to provide clarity.
        this._historicalData = 
        [
            10          : 4,
            100         : 25,
            1_000       : 168,
            10_000      : 1229,
            100_000     : 9592,
            1_000_000   : 78_498,
            10_000_000  : 664_579,
            100_000_000 : 5_761_455
        ];
    }

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
        const q = sqrt(_sieveSize.to!float).round.to!uint;

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
            iota(factor * 3, _sieveSize, factor * 2)
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
        foreach(num; 3.._sieveSize)
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
            _sieveSize,
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
        // .get(key, default_value)
        return this._historicalData.get(_sieveSize, uint.max) == this.countPrimes();
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

    private uint countPrimes() @nogc nothrow pure
    {
        import core.bitop : popcnt;
        import std.algorithm : map, fold;

        // D has a concept called `ranges`, which are (usually) lightweight structs
        // which provide a .popFront(), .front(), and .empty() functions.
        //
        // They are used to produce a range of values in a lazily-evaluated manner, often without
        // any/reduced heap allocations.
        //
        // The standard library provides a bunch of ranges which can be composed together to form
        // pipelines.
        //
        // `$` within a slice means "length", so 0..length (exclusive).
        //
        // So essentially all we're doing is making a lazily evaluated, allocationless pipeline:
        //  Take all the bytes we have from _bits.
        //  Evaluate popcnt(n) where `n` is the next byte.
        //  Find the sum of all the `popcnt(n)` evaluations.
        return this._bits[0..$]
                    .map!(num => popcnt(num & 0b1010_1010)) // We only care about odd numbers, so we mask out any bits that are even.
                    .fold!((a, b) => a + b)(0);
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
    s.printResults("BradleyChatha", "algorithm=base,faithful=yes,bits=1,parallel=no", 1, false, elapsedTime, passes);
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
        "algorithm=base,faithful=yes,bits=1,parallel=yes", 
        totalCPUs, 
        false, 
        elapsedTime, 
        passes
    );
}