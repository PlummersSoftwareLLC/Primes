# Microsoft BASIC solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)
![Deviation](https://img.shields.io/badge/Deviation-sievesize-blue)

This is based on the [Microsoft BASIC version 2 solution by davepl](../solution_4/README.md), but it has the following
optimizations:

- Only deal with odd numbers starting with 3
- Start the inner loop at `factor**2` instead of `2*factor`

The solution includes two BASIC source code files:

- primes.bas, which only runs the prime sieve but shows no output
- count.bas, which runs the prime sieve and then shows the number of primes found

The actual sieve code in both files is identical. The former file can be used to run a "clean" benchmark of the solution.
The second can be used to validate the result of the sieve.

## Run instructions

Build the docker image with this:

```bash
./build.sh
```

You should only need to do this once. Run the docker image:

```bash
./run.sh
```

## Output

```log
davepl-msbasic;249;5.001;1;algorithm=base,faithful=no
```
