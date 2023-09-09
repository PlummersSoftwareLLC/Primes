# Microsoft BASIC solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-yellowgreen)
![Deviation](https://img.shields.io/badge/Deviation-sievesize-blue)

This is based on the [Microsoft BASIC version 2 solution by davepl](../solution_4/README.md), but it has the following
optimizations:

- Only deal with odd numbers starting with 3
- Start the inner loop at `factor**2` instead of `2*factor` and step by `2*factor` instead of `factor`
- `1` is used for composite, and `0` is used for prime, so there is no need to initialize the sieve since it is
  automatically initialized to `0`

The solution includes four BASIC source code files:

- One bit per array item, sieve size 10000 
  - `primes1.bas` - Only runs the prime sieve but shows no output
  - `count1.bas` - Runs the prime sieve and then shows the number of primes found
- 16 bits per array item, sieve_size 100000
  - `primes2.bas` - Only runs the prime sieve but shows no output
  - `count2.bas` - Runs the prime sieve and then shows the number of primes found

The actual sieve code in each `count` files is identical to the corresponding `primes` file. The former can be used to
run a "clean" benchmark of the solution. The latter can be used to validate the result of the sieve.

NOTE: 16 bits is the maximum number of bits that can be stored in an integer, so effectively, `primes2.bas` is a 1-bit
solution.

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

On an Intel(R) Core(TM) i7-8700 CPU @ 3.20GHz with 32 GB of memory on a Windows 10 desktop running
a Ubuntu 22.04 VM in VirtualBox 6.1, the original solution by davepl had this result:


```log
davepl-msbasic;102;5.040;1;algorithm=base,faithful=no
```

This solution has this result on the same system:

```log
rzuckerm-msbasic-bool;402;5.011;1;algorithm=base,faithful=no
rzuckerm-msbasic-bit;8;5.368;1;algorithm=base,faithful=no,bits=1
```
