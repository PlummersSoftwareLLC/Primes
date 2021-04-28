using CommandLine;

namespace PrimeCSharp
{
    public class RunSettings
    {
        [Option('s', "size",
            Default = 1_000_000, HelpText = "The size of the sieve to run.")]
        public int SieveSize { get; set; }

        [Option('t', "time",
            Default = 5, HelpText = "How many seconds to run the sieve.")]
        public int SecondsToRun { get; set; }

        [Option('v', "verbose",
            Default = false, HelpText = "Show the prime numbers that were found.")]
        public bool ShowResults { get; set; }

        [Option('m', "multi",
            Default = false, HelpText = "Run the sieve in a multi-threaded manner.")]
        public bool MultiThreaded { get; set; }

        [Option("threads",
            Default = 0, HelpText = "Set the number of threads to use while multithreading. Setting to 0 will make it use the number of processors on the machine.")]
        public int ThreadCount { get; set; }

        [Option("pthreads",
            Default = 0, HelpText = "Set the number of threads to use while running parallel algorithms. Setting to 0 will make it use the number of processors on the machine.")]
        public int PThreadCount { get; set; }


        [Option("original",
            Default = false, HelpText = "Run the original version of the sieve.")]
        public bool Original { get; set; }

        [Option("standard",
            Default = false, HelpText = "Run the current standard version of the sieve. This is the default if nothing else is chosen.")]
        public bool Standard { get; set; }

        [Option("bool",
            Default = false, HelpText = "Run a sieve that uses an array of bools.")]
        public bool BoolArray { get; set; }

        [Option("ibool",
            Default = false, HelpText = "Run a sieve that uses an array of bools, but with inverted boolean logic to skip reinitialization of the array.")]
        public bool InvertedBoolArray { get; set; }

        [Option("dbool",
            Default = false, HelpText = "Run the inverted bool version, but inlining array references.")]
        public bool DirectInvertedBoolArray { get; set; }

        [Option("raw",
            Default = false, HelpText = "Run a sieve creates an array of bytes whose bits are directly accessed.")]
        public bool RawBits { get; set; }

        [Option("raw32",
            Default = false, HelpText = "Run a sieve creates an array of uints whose bits are directly accessed.")]
        public bool RawBits32 { get; set; }

        [Option("rawdirect",
            Default = false, HelpText = "Run a sieve that uses an array of bools.")]
        public bool RawBitsDirect { get; set; }

        [Option("pool",
            Default = false, HelpText = "Run a sieve that uses an array pool.")]
        public bool ArrayPool { get; set; }

        [Option("ref",
            Default = false, HelpText = "Run a sieve that uses an array pool. Ref Struct version.")]
        public bool ArrayPoolRef { get; set; }

        [Option("pool6",
            Default = false, HelpText = "Run a sieve that uses an array pool, and base 6 calculations.")]
        public bool ArrayPool2Of6 { get; set; }

        [Option("raw6",
            Default = false, HelpText = "Run a sieve that uses raw bit manipulation, and base 6 calculations.")]
        public bool RawBits2Of6 { get; set; }

        [Option("pool6p",
            Default = false, HelpText = "Run a sieve that uses an array pool, and base 6 calculations, in parallel.")]
        public bool ArrayPool6P { get; set; }

        [Option("rawp",
            Default = false, HelpText = "Run a parallel implementation of the rawbits sieve.")]
        public bool RawParallel { get; set; }

        [Option("pool30",
            Default = false, HelpText = "Run a sieve that uses an array pool, and base 30 calculations.")]
        public bool ArrayPool8of30 { get; set; }

        [Option("pool30m",
            Default = false, HelpText = "Run a sieve that uses an array pool, and base 30 calculations with custom bitmasking.")]
        public bool ArrayPool8of30M { get; set; }



        [Option("benchmark",
            Default = false, HelpText = "Run benchmarks on the sieve implementations.")]
        public bool Benchmark { get; set; }

        [Option('b', "bench",
            Default = "", HelpText = "A type of benchmark to run. Leaving it blank will run the standard benchmarks. Current options: mod, ref, ofN, par")]
        public string? Bench { get; set; }

        // Below added by repo maintainers to conform to drag race output format
        [Option('d', "dragrace",
            Default = "", HelpText = "Make output conform to dragrace format. This option only works with individual sieve runs (i.e., not with benchmarks)")]
        public bool Dragrace { get; set; }

    }
}
