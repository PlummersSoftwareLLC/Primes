# Assembly by DanielAtCosmicDNA

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-compile%20time-blue)

This solution uses Assembly language to compute as fast as possibly.

*Deviation*: It does use the base algorithm but due to compile time optimization, a lot of the actual calculation does not happen at runtime

*Faithfulness*: Since the buffer size is fixed, it is [not considered faithful](https://github.com/PlummersSoftwareLLC/Primes/pull/274).

*Note*: this solution may be limited to numbers up to around 50,000,000 (stack size limit on Mac OS it seems).

## Run instructions

`./run.sh`, requires CLANG in a fairly recent version

## Output

All on Dell M5510 - Intel(R) Xeon(R) CPU E3-1505M V5

### Native performance

#### Single-threaded

| Index | Implementation | Solution | Label      | Passes    | Duration | Threads | Algorithm | Faithful | Bits | Passes/Second |
|-------|----------------|----------|------------|-----------|----------|---------|-----------|----------|------|---------------|
| 1     | assembly       | 4        | cosmic_dna | 95053695  | 5.00002  | 1       | base      | no       | 1    | 19010678.16583 |


#### Multi-threaded
| Index | Implementation | Solution | Label      | Passes    | Duration | Threads | Algorithm | Faithful | Bits | Passes/Second |
|-------|----------------|----------|------------|-----------|----------|---------|-----------|----------|------|---------------|
| 1     | assembly       | 4        | cosmic_dna | 441098625 | 5.00319  | 8       | base      | no       | 1    | 11020436.79042 |

