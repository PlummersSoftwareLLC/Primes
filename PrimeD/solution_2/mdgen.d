#!/usr/bin/env dub
/+dub.sdl:
    name "mdgen"
    description "Generates part of the markdown file based off of the program's output"
+/
module mdgen;

import std;

const RUN_COMMAND = "dub run -b release --compiler=ldc2";
const MARKER = "<!--MDGEN_START-->";
const DUB_LINES = 4;
const INFO_REGEX = regex("^(.+);(.+);(.+);(.+);algorithm=(.+),bits=(.+),faithful=(.+)$");
                   // [1] = Tag. [2] = Passes. [3] = Time. [4] = Threads. [5] = Algorithm. [6] = Bits. [7] = Faithful

struct Output
{
    char[] stdout;
    char[] stderr;
}

struct Solution
{
    string      tag;
    ThreadMode  threadMode;
    ulong       passes;
    string      time;
    uint        threads;
    string      algorithm;
    uint        bits;
    bool        isFaithful;
}

enum ThreadMode
{
    single,
    staticThreads,
    dynamicThreads
}

void main()
{
    const file = readText("README.md");
    const cursor = findStartCursor(file);
    
    Appender!(char[]) output;
    output.put(file[0..cursor]); // Erase previous run's contents.
    output.put('\n');

    const runResult = runAndCollectOutput();
    const solutions = findSolutions(runResult);

    writeSolutionSection(output, solutions);
    writeOutputSection(output, runResult);
    std.file.write("README.md", output.data);
}

size_t findStartCursor(string file)
{
    writeln("Finding start cursor...");
    return indexOf(file, MARKER) + MARKER.length;
}

Output runAndCollectOutput()
{
    writeln("Running solution...");

    Output result;
    auto pipes = pipeShell(RUN_COMMAND, Redirect.stdout | Redirect.stderr);

    while(!tryWait(pipes.pid).terminated)
    {
        foreach(block; pipes.stdout.byChunk(4096))
            result.stdout ~= block;
        foreach(block; pipes.stderr.byChunk(4096))
            result.stderr ~= block;
    }

    return result;
}

Solution[] findSolutions(const Output result)
{
    writeln("Finding solutions...");
    return 
        result
        .stdout
        .lineSplitter
        .drop(DUB_LINES)
        .map!(line => line.matchFirst(INFO_REGEX))
        .tee!(match => assert(!match.empty))
        .map!(match => Solution(
            match.captures[1].splitter('-').drop(2).fold!((a,b) => a~b)(""),
            findThreadMode(match.captures[1]),
            match.captures[2].to!ulong,
            match.captures[3].idup,
            match.captures[4].to!uint,
            match.captures[5].idup,
            match.captures[6].to!uint,
            match.captures[7] == "yes"
        ))
        .array
        .multiSort!(
            (a, b) => a.isFaithful == b.isFaithful ? false : a.isFaithful,
            (a, b) => a.passes > b.passes
        )
        .array;
}

ThreadMode findThreadMode(const char[] tag)
{
    if(tag.canFind("-Single-"))
        return ThreadMode.single;
    else if(tag.canFind("-Multidynamic"))
        return ThreadMode.dynamicThreads;
    else if(tag.canFind("-Multistatic"))
        return ThreadMode.staticThreads;

    assert(false, tag);
}

string findDescription(string tag)
{
    auto descriptions = 
    [
        "SieveRT":              "Sieve that where everything is allocated and computed at runtime.",
        "SieveRT_8":            "An 8-bit variant of SieveRT.",
        "SieveCT":              "Sieve where some storage is statically allocated, and some calcs are compile-time evaluated.",
        "SieveRT_LookupTable":  "Sieve that uses a compile-time generated lookup table.",
        "SieveRTCT_Cheatiness": "Sieve that is fully generated at compile-time, which is kind of cheating."
    ];

    return descriptions.get(tag, "None given for this solution.");
}

void writeOutputSection(ref Appender!(char[]) output, const Output result)
{
    writeln("Writing output section");

    output.put("## Output\n\n");
    output.put("```\n");
    output.put("Command: "); output.put(RUN_COMMAND);
    output.put("\nstderr:\n");
    foreach(line; result.stderr.lineSplitter)
    {
        output.put("    "); output.put(line); output.put('\n');
    }
    output.put("\nstdout:\n");
    foreach(line; result.stdout.lineSplitter.drop(DUB_LINES))
    {
        output.put("    "); output.put(line); output.put('\n');
    }
    output.put("```\n\n");
}

void writeSolutionSection(ref Appender!(char[]) output, const Solution[] solutions)
{
    writeln("Writing solutions section");

    output.put("## Solutions\n\n");
    output.put("| Tag | Description | Multithreaded | Passes | Algorithm | Bits | Faithful |\n");
    output.put("|-----|-------------|---------------|--------|-----------|------|----------|\n");
    foreach(solution; solutions)
    {
        output.put("| ");   output.put(solution.tag);                       output.put(" | ");
                            output.put(findDescription(solution.tag));      output.put(" | ");
                            output.put(solution.threadMode.to!string);      output.put(" | ");
                            output.put(solution.passes.to!string);          output.put(" | ");
                            output.put(solution.algorithm);                 output.put(" | ");
                            output.put(solution.bits.to!string);            output.put(" | ");
                            output.put(solution.isFaithful.to!string);      output.put(" |\n");
    }
    output.put("\n\n");
}