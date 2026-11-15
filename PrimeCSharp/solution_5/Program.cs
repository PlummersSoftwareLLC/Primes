using System;
using System.Diagnostics;
using System.Globalization;
using PrimeSieveCS;

const int sieveSize = 1_000_000;
const int expectedCount = 78_498;
const int defaultSegment = 32 * 1024;
const string baseLabel = "pbednarcik";

// Options for experiments. The defaults are what the benchmark runs.
double warmupSeconds = 1;                           // --warmup <seconds>
int segmentBytes = defaultSegment;                  // --segment <bytes>, 0 = unsegmented
int vectorMax = PrimeSieve.DefaultVectorDenseMax;   // --vecmax <factor>, 0 = no vectors
bool stats = false;                                 // --stats, GC counts to stderr

for (int i = 0; i < args.Length; i++)
{
    bool hasValue = i + 1 < args.Length;
    if (args[i] == "--warmup" && hasValue)
        warmupSeconds = double.Parse(args[++i], CultureInfo.InvariantCulture);
    else if (args[i] == "--segment" && hasValue)
        segmentBytes = int.Parse(args[++i]);
    else if (args[i] == "--vecmax" && hasValue)
        vectorMax = int.Parse(args[++i]);
    else if (args[i] == "--stats")
        stats = true;
}

// A non-default setting gets its own label, so results can't be mixed up.
string label = baseLabel;
if (segmentBytes == 0)
    label += "-unsegmented";
else if (segmentBytes != defaultSegment)
    label += $"-seg{segmentBytes / 1024}k";
if (vectorMax != PrimeSieve.DefaultVectorDenseMax)
    label += $"-vec{vectorMax}";

// Let the JIT finish before the clock starts.
long warmupEnd = Stopwatch.GetTimestamp() + (long)(warmupSeconds * Stopwatch.Frequency);
while (Stopwatch.GetTimestamp() < warmupEnd)
    Run();

int gen0Before = GC.CollectionCount(0);
TimeSpan pauseBefore = GC.GetTotalPauseDuration();

// The timed run: a new sieve every pass, for 5 seconds.
long budget = 5 * Stopwatch.Frequency;
long started = Stopwatch.GetTimestamp();
long elapsed;
int passes = 0;
PrimeSieve sieve;
do
{
    sieve = Run();
    passes++;
    elapsed = Stopwatch.GetTimestamp() - started;
} while (elapsed < budget);

double seconds = (double)elapsed / Stopwatch.Frequency;

if (stats)
{
    int gen0 = GC.CollectionCount(0) - gen0Before;
    double pauseMs = (GC.GetTotalPauseDuration() - pauseBefore).TotalMilliseconds;
    Console.Error.WriteLine($"stats: gen0={gen0} gcPauseMs={pauseMs:F1} warmup={warmupSeconds}");
}

int count = sieve.CountPrimes();
if (count != expectedCount)
{
    Console.Error.WriteLine($"Invalid result: counted {count} primes up to {sieveSize}, expected {expectedCount}.");
    return 1;
}

Console.WriteLine($"{label};{passes};{seconds:G6};1;algorithm=base,faithful=yes,bits=1");
return 0;

PrimeSieve Run()
{
    var s = new PrimeSieve(sieveSize, vectorMax);
    return segmentBytes == 0 ? s.RunSieve() : s.RunSieveSegmented(segmentBytes);
}
