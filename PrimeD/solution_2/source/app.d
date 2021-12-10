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
final class SieveCT(size_t SieveSize)
if(SieveSize > 0) // We can attach constraints onto templated things that must succeed, otherwise the compiler will raise an error.
{
    // Because there's two sieves (compile-time+runtime, and a purely runtime one), they
    // share most of their functionality.
    //
    // This is explained more at the actual definition of `CommonSieveFunctions`, which is defined after this class.
    mixin CommonSieveFunctions!8;

    // You can apply qualifiers and attributes in a block, kind of like `public:` from C/C++, 
    // but less annoying.
    private
    {
        // Because SieveSize is a templated value, we know what it is at compile-time, so can stack allocate here.
        // Most D functions can be ran at compile-time. This is called Compile Time Function Execution (CTFE).
        // `alignTo` is being used in CTFE here.
        ubyte[alignTo!8(SieveSize) / 8] _bits;
    }

    // We'll use this helper func at compile-time to ensure that the user passes
    // a valid value into the `SieveSize` template parameter.
    //
    // This is needed because `validateResults` only works on these specific values.
    static bool isValidSieveSize(size_t size)
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
    private bool validateResults() nothrow pure inout // "inout" means, in a nutshell, "can be run from either a mutable or const or immutable reference".
    {
        // We can embed structs directly into functions.
        // By marking it 'static' we're saying that it doesn't need access to the function context.
        static struct PrimeCountPair
        {
            size_t primeLimit;
            size_t primeCount;
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
mixin template CommonSieveFunctions(size_t bitSize)
{   
    // D allows nesting imports into _any_ scope, in order to avoid symbol pollution, and
    // it also allows D to write more "file-portable" code.
    import core.time : Duration;

    // You can of course also do the C/C++ style to avoid a level of indentation.
    // `@safe` is a built-in attribute that specifies "the compiler can guarentee(citation needed) this code is memory safe".
    @safe:

    mixin RunSieve;

    private bool getBit(size_t index) @nogc nothrow inout
    {
        // Fairly standard bitty stuff.
        assert(index % 2 == 1, "Index is even?");
        return (this._bits[index / bitSize] & (1UL << (index % bitSize))) != 0;
    }

    private void setBit(size_t index) @nogc nothrow
    {
        this._bits[index / bitSize] |= 1UL << (index % bitSize);
    }

    private size_t countPrimes() nothrow inout
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
        return iota(3, SieveSize, 2)
                .map!(num => this.getBit(num) ? 0 : 1) // Ternary operator is just to make it more clear, not actually needed.
                .sum + 1; // + 1 is to account for '2' being a special case.
    }
}

mixin template RunSieve()
{
    import core.time : Duration;

    // Members are public by default.
    // Because this function is injected into both `SieveCT` and `SieveRT`, the compiler can actually
    // perform any optimisations it wants in `SieveCT` based off the fact that `SieveSize` is known at compile-time,
    // while performing different optimisations in `SieveRT` due to it only being known at run-time.
    void runSieve()
    {
        // We can also selectively specify which symbols from a package we want to import.
        // It's good practice to do so (less symbol bloat), but it's also super tedious.
        import std.algorithm : each;
        import std.math      : sqrt, round;
        import std.range     : iota;

        // Something to note about the below code:
        //      We're using the `sqrt` function from `std.math` using member function syntax?
        //      That's because D has a concept called UFCS (Uniform Function Call Syntax)
        //      Most function calls written as `func(a, b, c)` can be written as `a.func(b, c)`.
        //
        // Also, in D we pass template parameters via `!(templateP1, templateP2)(runtimeP1, runtimeP2)`. 
        // If there's only one template parameter, then you can usually use the form `!templateP(runtimeP1, runtimeP2)`
        //
        // Also, also, if a function doesn't need any runtime parameters, you can just completely omit the parenthesis:
        //  `.sqrt()` and `.sqrt` are exactly the same.
        //
        // Normally one would use `std.conv.to` to do casting, e.g. `sqrt(123).to!size_t` but it was generating
        // inefficient ASM, so I'm now using raw casts.

        auto factor = 3UL;
        const q = cast(size_t)((cast(double)SieveSize).sqrt);

        while(factor <= q)
        {
            if(this.getBit(factor))
            {
                factor += 2;
                continue;
            }
        
            enum LOOP_UNROLL_FACTOR = 64;
            auto num = factor * factor;
            while(num < SieveSize)
            {
                if(num + (factor * LOOP_UNROLL_FACTOR) < SieveSize)
                {
                    static foreach(i; 0..LOOP_UNROLL_FACTOR / 2)
                    {
                        this.setBit(num);
                        num += factor * 2;
                    }
                }
                else
                {
                    this.setBit(num);
                    num += factor * 2;
                }
            }

            factor += 2;
        }
    }

    void printResults(
        string tag,
        string attribs,
        size_t threadCount, 
        bool showResults,
        Duration duration,
        size_t passes
    ) @trusted inout // `stderr` is unsafe apparently
    {
        import std.array  : Appender;
        import std.conv   : to;
        import std.stdio  : writeln, writefln, stderr;
        import std.format : format;
        import std.range  : iota;

        // Similar to `StringBuilder` from C#
        Appender!(char[]) output;

        if(showResults)
            output.put("2, ");

        auto count = 1;
        foreach(num; iota(3, SieveSize, 2))
        {
            if(!this.getBit(num))
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
}

// This is the faithful, runtime-based version of `SieveRT`
final class SieveRT
{
    mixin CommonSieveFunctions!8;

    private ubyte[] _bits;
    private size_t _sieveSize;

    // The functions in `CommonSieveFunctions` expects the sieve size to be called `SieveSize`, not `_sieveSize`,
    // so we can make an alias to it in order to keep both naming conventions.
    // It's very much up to debate on how this should be handled, but this is just an easy, D-ish way forward.
    private alias SieveSize = _sieveSize;

    @safe:

    // In D, we use `this` as the name of constructors. Makes sense really.
    this(size_t sieveSize)
    {
        this._bits.length = alignTo!8(sieveSize) / 8;
        this._sieveSize = sieveSize;
    }

    // This is super super forced, but I couldn't think of anywhere else to try and show off this feature.
    // We'll be using `PrimePair` as a UDA (User defined attribute) to define half of the values
    // of the historical prime list. It's hard trying to fit so many different things into a small problem T_T
    static struct PrimePair
    {
        size_t sieveSize;
        size_t primeCount;
    }

    @PrimePair(10_000, 1229)
    @PrimePair(1_000,  168)
    @(PrimePair(100, 25), PrimePair(10, 4)) // alternate syntax.
    private bool validateResults() inout
    {
        import std.algorithm : filter, map;
        import std.traits    : getUDAs;

        // Create an associative array. I've used explicit typing to show what the AA type in
        // D is defined as: VALUE_TYPE[KEY_TYPE]
        size_t[size_t] primeList = 
        [
            100_000_000 : 5_761_455,
            10_000_000  : 664_579,
            1_000_000   : 78_498,
            100_000     : 9592
        ];

        // `enum` here means `value that only exists at compile-time, and does not take up stack/memory space`
        // Essentially all we're saying is `Find all the @PrimePair UDAs on the validateResults function`
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

// This is a *super* unfaiathful version which computes the sieve
// at compile-time. Honestly this is just testing how fast your CPU can construct an empty class.
// "but da C++ solution" is my excuse!
// RTCT = Runtime sieve at Compile Time.
final class SieveRTCT_Cheatiness(size_t SieveSize)
{
    import core.time : Duration;

    // `static` means the variable exists without need of an object in memory (i.e embedded within global memory/the exe)
    // So if we do something like say, call a function to assign its value, because the compiler
    // needs to know what the actual value is beforehand, it'll execute this function at compile-time.
    //
    // Essentially this is just running the sieve at compile time to pregenerate it.
    static const Sieve = (){
        auto sieve = new SieveRT(SieveSize);
        sieve.runSieve();
        return sieve;
    }();

    // Explained further down.
    this(){}
    this(size_t){}

    // I wanted to do the below thing, but I think I'm hitting a compiler bug.
    // enum IsValid = Sieve.validateResults();

    // Well... It's precomputed so we just no-op this.
    void runSieve()
    {
    }

    void printResults(
        string tag,
        string attribs,
        size_t threadCount, 
        bool showResults,
        Duration duration,
        size_t passes
    ) @trusted // `stderr` is unsafe apparently
    {
        Sieve.printResults(tag, attribs, threadCount, showResults, duration, passes);
    }
}

final class SieveRT_LookupTable(size_t SieveSize)
{
    import core.time : Duration;

    // Since we've already defined a compile-time evaluated sieve, we don't need to bother making it again.
    alias Sieve = SieveRTCT_Cheatiness!SieveSize.Sieve;
    size_t count;

    // ditto.
    this(){}
    this(size_t){}

    void runSieve()
    {
        import std.algorithm : count;
        import std.range : iota;

        this.count = 
            iota(3, SieveSize, 2)
            .count!(num => (Sieve._bits[num / 8] & (1 << (num % 8))) > 0);
    }

    void printResults(
        string tag,
        string attribs,
        size_t threadCount, 
        bool showResults,
        Duration duration,
        size_t passes
    ) @trusted // `stderr` is unsafe apparently
    {
        assert(this.count == Sieve.countPrimes());
        Sieve.printResults(tag, attribs, threadCount, showResults, duration, passes);
    }
}

final class SieveRTBX(alias BitT)
{
    mixin RunSieve; 

    private BitT[] _bits;
    private size_t _sieveSize;
    private alias SieveSize = _sieveSize;

    @safe:

    this(size_t sieveSize)
    {
        this._bits.length = sieveSize/2;
        this._sieveSize = sieveSize;
    }

    private bool getBit(size_t index) nothrow inout
    {
        return this._bits[index/2] == 1;
    }

    private void setBit(size_t index) @nogc nothrow
    {
        this._bits[index/2] = 1;
    }

    private size_t countPrimes() nothrow inout
    {
        import std.algorithm : filter, map;
        import std.range : walkLength, iota;

        // Slightly different way instead of using .count
        return iota(3, SieveSize, 2)
                .map!(num => !this.getBit(num))
                .filter!(b => b)
                .walkLength + 1;
    }

    private bool validateResults() inout
    {
        auto sieve = new SieveRT(this._sieveSize); // Since I know this one validates correctly. 
        sieve.runSieve();
        return this.countPrimes() == sieve.countPrimes();
    }
}

final class SieveCT_MegaUnroll(size_t SieveSize)
{
    mixin CommonSieveFunctions!8;

    private ubyte[alignTo!8(SieveSize) / 8] _bits;

    bool validateResults() inout
    {
        return true;
    }

    void runSieve()
    {
        import std.algorithm : each;
        import std.math      : sqrt, round;
        import std.range     : iota;

        const q = cast(size_t)((cast(double)SieveSize).sqrt);

        static foreach(factor; iota(3, q, 2))
        {
            if(!this.getBit(factor))
            {
                static foreach(i; iota(factor * factor, SieveSize, factor * 2))
                {
                    this.setBit(i);
                }
            }
        }
    }
}

// What if we could... say... generate a string at compile time and then use that string as code?
string generateSieveRT(alias BitType)()
{
    import std.format : format;

    // q{} are token strings: Strings that must evaluate to D tokens.
    return format!q{
    final class SieveRTB1_%s
    {
        mixin CommonSieveFunctions!%s;

        private %s[] _bits;
        private size_t _sieveSize;
        private alias SieveSize = _sieveSize;

        @safe:

        this(size_t sieveSize)
        {
            this._bits.length = alignTo!%s(sieveSize) / %s;
            this._sieveSize = sieveSize;
        }

        private bool validateResults() inout
        {
            auto sieve = new SieveRT(this._sieveSize);
            sieve.runSieve();
            return this.countPrimes() == sieve.countPrimes();
        }
    }
    }(
        BitType.sizeof * 8,
        BitType.sizeof * 8,
        BitType.stringof,
        BitType.sizeof * 8,
        BitType.sizeof * 8,
    );
}
mixin(generateSieveRT!ushort);
mixin(generateSieveRT!uint);
mixin(generateSieveRT!ulong);

// ditto, but for running them!
// But.. what if we got the format string from an external file first?
immutable RUN_SIEVE_FORMAT = import("run_sieve.d"); // String import paths are relative to the /views/ folder.
immutable RUN_SIEVE_LEADERBOARD_FORMAT = import("run_sieve_leaderboard.d");
string generateSieveRTRunner(string Alias, alias BitType, bool UseLeaderboardVersion = false)()
{
    import std.format : format;

    // Now, I didn't promise maintainable code, but I think I've snuck in the vast majority of D's cooler features.
    const bits = BitType.sizeof * 8;
    
    static if(UseLeaderboardVersion)
    {
        return format!RUN_SIEVE_LEADERBOARD_FORMAT(
            Alias, bits,
            Alias, 1,
        );
    }
    else
    {
        return format!RUN_SIEVE_FORMAT(
            Alias, bits,
            Alias, 1,
            Alias, 1,
            Alias, 1
        );
    }
}

version(Prime_100_000)
    enum PRIME_COUNT = 100_000;
else
    enum PRIME_COUNT = 1_000_000;


enum MAX_SECONDS = 5;

import std.typecons : Flag, Yes, No;

// Create a type-safe boolean. `Yes.faithful`, `No.faithful`, `IsFaithful.yes`, `IsFaithful.no`, can all be used.
alias IsFaithful = Flag!"faithful";

void main(string[] args)
{
    import std.getopt;

    enum Mode
    {
        all = 100,
        leaderboard = 10,
    }

    Mode mode;

    auto result = getopt(args,
        config.required, "mode|m", "all,leaderboard", &mode,
    );

    if(result.helpWanted)
    {
        defaultGetoptPrinter("", result.options);
        return;
    }

    alias dt = MultithreadMode.dynamicThreads;
    alias st = MultithreadMode.staticThreads;

    final switch(mode) with(Mode)
    {
        case leaderboard:
            alias s1 = SieveCT!PRIME_COUNT;
            runSingleThreaded!s1(IsFaithful.no);
            runMultiThreaded!(s1, st)(No.faithful);

            alias s2 = SieveRT;
            runSingleThreaded!s2(IsFaithful.yes);
            runMultiThreaded!(s2, st)(Yes.faithful);

            alias s3 = SieveRTBX!ubyte;
            runSingleThreaded!s3(IsFaithful.yes, "base", 8);

            alias s4 = SieveRTCT_Cheatiness!PRIME_COUNT;
            runMultiThreaded!(s4, st)(IsFaithful.no, "other", 1);

            mixin(generateSieveRTRunner!("s5", ushort, true));
            mixin(generateSieveRTRunner!("s6", uint, true));
            mixin(generateSieveRTRunner!("s7", ulong, true));

            alias s8 = SieveRTB1_32;
            runMultiThreaded!(s8, st)(IsFaithful.yes, "base", 1); // 32 has the best performance on my machine, so I'll use that for the multithreaded leaderboard.

            // This one is here just to have a "non-bool yet used as a bool" version there.
            alias s9 = SieveRTBX!ulong;
            runSingleThreaded!s9(IsFaithful.yes, "base", 64);

            version(CompileUnrolled)
            {
                alias s10 = SieveCT_MegaUnroll!PRIME_COUNT;
                runSingleThreaded!s10(IsFaithful.no);
            }
            break;

        case all:
            alias s1 = SieveCT!PRIME_COUNT;
            runSingleThreaded!s1(IsFaithful.no);
            runMultiThreaded!(s1, st)(No.faithful);
            runMultiThreaded!(s1, dt)(No.faithful); // Same thing as IsFaithful.no

            alias s2 = SieveRT;
            runSingleThreaded!s2(IsFaithful.yes);
            runMultiThreaded!(s2, st)(Yes.faithful);
            runMultiThreaded!(s2, dt)(Yes.faithful);

            alias s3 = SieveRTCT_Cheatiness!PRIME_COUNT;
            runSingleThreaded!s3(IsFaithful.no & IsFaithful.no & IsFaithful.no, "other", 1);
            runMultiThreaded!(s3, st)(IsFaithful.no, "other", 1);
            runMultiThreaded!(s3, dt)(IsFaithful.no, "other", 1);

            alias s4 = SieveRT_LookupTable!PRIME_COUNT;
            runSingleThreaded!s4(IsFaithful.no, "lookup", 1);
            runMultiThreaded!(s4, dt)(IsFaithful.no, "lookup", 1);
            runMultiThreaded!(s4, st)(IsFaithful.no, "lookup", 1);

            alias s5 = SieveRTBX!ubyte;
            runSingleThreaded!s5(IsFaithful.yes, "base", 8);
            runMultiThreaded!(s5, dt)(IsFaithful.yes, "base", 8);
            runMultiThreaded!(s5, st)(IsFaithful.yes, "base", 8);

            mixin(generateSieveRTRunner!("s6", ushort));
            mixin(generateSieveRTRunner!("s7", uint));
            mixin(generateSieveRTRunner!("s8", ulong));

            alias s9 = SieveRTBX!ushort;
            runSingleThreaded!s9(IsFaithful.yes, "base", 16);
            runMultiThreaded!(s9, dt)(IsFaithful.yes, "base", 16);
            runMultiThreaded!(s9, st)(IsFaithful.yes, "base", 16);

            alias s10 = SieveRTBX!ushort;
            runSingleThreaded!s10(IsFaithful.yes, "base", 32);
            runMultiThreaded!(s10, dt)(IsFaithful.yes, "base", 32);
            runMultiThreaded!(s10, st)(IsFaithful.yes, "base", 32);

            alias s11 = SieveRTBX!ushort;
            runSingleThreaded!s11(IsFaithful.yes, "base", 64);
            runMultiThreaded!(s11, dt)(IsFaithful.yes, "base", 64);
            runMultiThreaded!(s11, st)(IsFaithful.yes, "base", 64);
            break;
    }
}

// Here we're asking for an alias to another symbol, so we can pass in either of the sieve types.
void runSingleThreaded(alias SieveType)(IsFaithful faithful, string algorithm = "base", uint bits = 1)
{
    import std.algorithm          : canFind;
    import std.conv               : to;
    import std.datetime.stopwatch : StopWatch, AutoStart;
    import std.format             : format;

    auto passes = 1u;
    auto timer = StopWatch(AutoStart.yes);

    // One interesting usage of templates: Specifying a unit to act on.
    while(timer.peek.total!"seconds" < MAX_SECONDS)
    {
        // We have one problem, if `SieveType` is an instance of `SieveCT`, then
        // it doesn't have a constructor taking a `size_t`.
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
        // (Also, since the other sieves were added a bit later I didn't account for them in this code.
        //  So they actually manage to be matched as both the RT and CT versions at different points
        //  so my solution was to just give them both types of constructors instead of complicating things further)

        // #1: Using the `is()` expression on a concrete type.
        // #5: We can also execute some code to make it even more generic.
        static if(
            /*#1*/ is(SieveType == SieveRT) || is(SieveType == SieveRTBX!ubyte)
            /*#5*/ || __traits(identifier, SieveType).canFind("RT")
        )
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
    //   "is SieveType a SieveCT with one template parameter Param1 where Param1 is a size_t?"
    //   (this also extracts the template parameter as `Param1` so you can evaluate it and use it for more shenanigans)
    static if(is(SieveType == SieveCT!Param1, size_t Param1) || is(SieveType == SieveCT_MegaUnroll!Param1, size_t Param1))
        auto s = new SieveType();
    else
        auto s = new SieveType(PRIME_COUNT);

    s.runSieve();
    s.printResults(
        "BradleyChatha-Single-%s-%sbit".format(SieveClassName, bits), 
        "algorithm=%s,bits=%s,faithful=%s".format(algorithm, bits, faithful), // Flag.to!string -> "yes" or "no". 
        1, 
        false, 
        elapsedTime, 
        passes
    );
}

enum MultithreadMode
{
    dynamicThreads, // Constantly create threads for the full 5 seconds.
    staticThreads   // Create only totalCPUs amount of threads that run for the full 5 seconds.
}

void runMultiThreaded(alias SieveType, MultithreadMode ThreadMode)(IsFaithful faithful, string algorithm = "base", uint bits = 1)
{
    import core.atomic            : atomicOp;
    import std.conv               : to;
    import std.datetime.stopwatch : StopWatch, AutoStart;
    import std.format             : format;
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
        static if(ThreadMode == MultithreadMode.dynamicThreads)
        {
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
        else static if(ThreadMode == MultithreadMode.staticThreads)
        {
            foreach(i; iota(0, totalCPUs).parallel)
            {
                uint myPasses;
                while(timer.peek.total!"seconds" < MAX_SECONDS)
                {
                    static if(__traits(compiles, new SieveType(PRIME_COUNT)))
                        scope sieve = new SieveType(PRIME_COUNT);
                    else
                        scope sieve = new SieveType();
                    sieve.runSieve();
                    myPasses++;
                }
                atomicOp!"+="(passes, myPasses);
            }
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
        "BradleyChatha-Multi%s-%s-%sbit".format(ThreadMode, __traits(identifier, SieveType), bits), 
        "algorithm=%s,bits=%s,faithful=%s".format(algorithm, bits, faithful),
        totalCPUs, 
        false, 
        elapsedTime, 
        passes
    );
}
