# Java Prime Sieve

## Description

A slightly more optimized version of the original algorithm removing divisions, branches and using a boolean array instead of the BitSet class

## Execution and requirements

A Java Version of 8+ (Probably works with 6+) installed is required to run the program.
Execution can be done using the run-sieve.sh script which first compiles the java file and then executes the class

### Command Line Arguments

| Name                | Description                                                  |
| ------------------- | ------------------------------------------------------------ |
| `--seconds=<value>` | The execution time in seconds. Accepts floating point values |
| `--limit=<value>`   | The upper limit of the sieve. Must be an integer             |

## Specs

Running the program in shell resulted in significantly lower performance than in my IDE (Eclipse) for some reason, though execution time scales linearly with time:
| Runtime | Sieve Limit | Shell | IDE   |
| ------- | ----------- | ----- | ----- |
| 1s      | 1.000.000   | 380   | 950   |
| 2s      | 1.000.000   | 760   | 2150  |
| 5s      | 1.000.000   | 1900  | 5480  |
| 10s     | 1.000.000   | 3810  | 11100 |

All benchmarks ran on an Intel(R) Core(TM) i7-8700K CPU @ 3.70GHz