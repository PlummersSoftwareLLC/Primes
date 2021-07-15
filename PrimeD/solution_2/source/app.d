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
// CT = Compile-Time, because this version uses compile-time stuff to reduce some of the work it has to do.
//
// `SieveCT` is unfaithful, `SieveRT` is faithful.
final class SieveCT(uint SieveSize)
if(SieveSize > 0) // We can attach constraints onto templated things that must succeed, otherwise the compiler will raise an error.
{
    // Because there's two sieves (compile-time+runtime, and a purely runtime one), they
    // share most of their functionality.
    //
    // This is explained more at the actual definition of `CommonSieveFunctions`, which is defined after this class.
    mixin CommonSieveFunctions;

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
    // a valid value into the `SieveSize` template parameter.
    //
    // This is needed because `validateResults` only works on these specific values.
    static bool isValidSieveSize(uint size)
    {
        import std.algorithm : canFind;
        return [10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000].canFind(size);
    }

    // This then allows us to perform our own compile-time checks with custom error messages!
    static assert(isValidSieveSize(SieveSize), "You have provided an invalid sieve size.");

    // The CT and RT versions will define their own `validateResults` function, because this lets me show off a few
    // compile-time things D can do.
    //
    // Since this function is only ever called after all data mutations are done, it seems appropriate to state that it's `pure`.
    private bool validateResults() nothrow pure
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
            // Conditionally compile code.
            static if(SieveSize == pair.primeLimit)
                return this.countPrimes() == pair.primeCount;
        }
    }
}

// Remember that `mixin CommonSieveFunctions` line at the start of the `SieveCT` class?
// Essentially what happens is, the compiler will take every member of this `mixin template` and inject it
// straight into the class.
//
// So it's kind of like a multi-line #define, in a way.
//
// A `mixin template` is a little overkill compared to making something like a base `Sieve` class, but I wanted
// to shoe-horn as many of D's cool features as I could.
//
// Mixin templates have some pretty cool uses, but this is one of the simpler ways to use them.
mixin template CommonSieveFunctions()
{   
    // D allows nesting imports into _any_ scope, in order to avoid symbol pollution, and
    // it also allows D to write more "file-portable" code.
    import core.time : Duration;

    // You can of course also do the C/C++ style to avoid a level of indentation.
    // `@safe` is a built-in attribute that specifies "the compiler can guarentee(citation needed) this code is memory safe".
    @safe:

    // Members are public by default.
    // Because this function is injected into both `SieveCT` and `SieveRT`, the compiler can actually
    // perform any optimisations it wants in `SieveCT` based off the fact that `SieveSize` is known at compile-time,
    // while performing different optimisations in `SieveRT` due to it only being known at run-time.
    void runSieve()
    {
        // We can also selectively specify which symbols from a package we want to import.
        // It's good practice to do so (less symbol bloat), but it's also super tedious.
        import std.algorithm : each;
        import std.conv      : to;
        import std.math      : sqrt, round;
        import std.range     : iota;

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

    private uint countPrimes() nothrow
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
        // Then:
        //  Evalulate all values of the pipeline, and find the sum of all the mapped results.
        return iota(0, SieveSize)
                .map!(num => this.getBit(num) ? 1 : 0) // Ternary operator is just to make it more clear, not actually needed.
                .sum;
    }
}

// This is the faithful, runtime-based version of `SieveRT`
final class SieveRT
{
    mixin CommonSieveFunctions;

    private ubyte[] _bits;
    private uint _sieveSize;

    // The functions in `CommonSieveFunctions` expects the sieve size to be called `SieveSize`, not `_sieveSize`,
    // so we can make an alias to it in order to keep both naming conventions.
    // It's very much up to debate on how this should be handled, but this is just an easy, D-ish way forward.
    private alias SieveSize = _sieveSize;

    // In D, we use `this` as the name of constructors. Makes sense really.
    this(uint sieveSize)
    {
        this._bits.length = alignTo!8(sieveSize) / 8;
        this._bits[] = ubyte.max;
        this._sieveSize = sieveSize;
    }

    private bool validateResults()
    {
        import std.algorithm : filter, map;

        // Create an associative array. I've added the extra typing to show what the AA type in
        // D is defined as: VALUE_TYPE[KEY_TYPE]
        const uint[uint] primeList = 
        [
            100_000_000 : 5_761_455,
            10_000_000  : 664_579,
            1_000_000   : 78_498,
            100_000     : 9592,
            10_000      : 1229,
            1_000       : 168,
            100         : 25,
            10          : 4,
        ];

        const primes = this.countPrimes();
        if(this._sieveSize in primeList)
            return primeList[this._sieveSize] == primes;
        else
            return false; // TODO: Figure out how to determine validity for any arbitrary sieve size.
    }
}

// `enum` here means `value that only exists at compile-time, and does not take up stack/memory space`
enum PRIME_COUNT = 1_000_000;
enum MAX_SECONDS = 5;

import std.typecons : Flag, Yes, No;

// Create a type-safe boolean. `Yes.faithful`, `No.faithful`, `IsFaithful.yes`, `IsFaithful.no`, can all be used.
alias IsFaithful = Flag!"faithful";

void main()
{
    runSingleThreaded!(SieveCT!PRIME_COUNT)(IsFaithful.no);
    runMultiThreaded!(SieveCT!PRIME_COUNT)(No.faithful); // Same thing as IsFaithful.no
    runSingleThreaded!SieveRT(IsFaithful.yes);
    runMultiThreaded!SieveRT(Yes.faithful);
}

// Here we're asking for an alias to another symbol, so we can pass in either of the sieve types.
void runSingleThreaded(alias SieveType)(IsFaithful faithful)
{
    import std.conv               : to;
    import std.datetime.stopwatch : StopWatch, AutoStart;

    auto passes = 1u;
    auto timer = StopWatch(AutoStart.yes);

    // One interesting usage of templates: Specifying a unit to act on.
    while(timer.peek.total!"seconds" < MAX_SECONDS)
    {
        // We have one problem, if `SieveType` is an instance of `SieveCT`, then
        // it doesn't have a constructor taking a `uint`.
        //
        // If we have `SieveType` of `SieveRT`, then we *do* have that constructor.
        //
        // That means we need to determine what `SieveType` is in order to select
        // the right way to construct things.
        //
        // I'll show a different solution each time we need to make a sieve.
        //
        // (We could also just add a dummy ctor in `SieveCT`, but that's booooring)
        // (We also could've made a function specifically for constructing the sieves, but
        //  then I couldn't show off D as much!)

        // #1: Using the `is()` expression on a concrete type.
        static if(is(SieveType == SieveRT))
            scope sieve = new SieveType(PRIME_COUNT);
        else
            scope sieve = new SieveType();

        sieve.runSieve();
        passes++;
    }

    const elapsedTime    = timer.peek();
    const SieveClassName = __traits(identifier, SieveType); // __traits lets us learn a *lot* of useful info from the compiler, such as symbol names.

    // #2: Using the `is()` expression on a template type.
    //     This is one of the more arcane forms of `is()`, so it's often wrapped up inside a more user-friendly thing.
    //     More reading of `is()`: https://dlang.org/spec/expression.html#IsExpression
    //
    // A very brief attempt of explaining it is:
    //   "is SieveType a SieveCT with one template parameter Param1 where Param1 is a uint?"
    //   (this also extracts the template parameter as `Param1` so you can evaluate it and use it for more shenanigans)
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
        // .parallel makes the foreach body run on worker threads. It defaults to using as many
        // threads as there are CPUs (a.k.a totalCPUs).
        //
        // D makes multi-threaded processing painfully easy.
        foreach(i; iota(0, totalCPUs).parallel)
        {
            // #3: Using __traits(compiles) to determine if we need to pass a value to the ctor or not.
            static if(__traits(compiles, new SieveType(PRIME_COUNT)))
                scope sieve = new SieveType(PRIME_COUNT);
            else
                scope sieve = new SieveType();
            sieve.runSieve();
            atomicOp!"+="(passes, 1);
        }
    }

    const elapsedTime = timer.peek();
    
    // #4: Use `std.traits.isInstanceOf` to determine if `SieveType` is an instance of `SieveCT`.
    //     This is basically a user friendly version of that weirder `is()` statement.
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
