# Microsoft BASIC solution by davepl and rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-sievesize-blue)

This is an implementation in Mirosoft BASIC version 2, as available and used on many home computers in the 1970s and '80s.

As the systems in question typically had a maximum of 64KiB of total system RAM and CPUs that couldn't address more
than that, this implementation uses a sieve size of 10,000 for the one-bit per array item solution, and 100,000 for the
16 bits per array item solution.

The solution includes four BASIC source code files:

- One bit per array item, sieve size 10,000:
  - `primes.bas` - Only runs the prime sieve but shows no output
  - `count.bas` - Runs the prime sieve and then shows the number of primes found and the expected number of primes
- 16 bits per array item, sieve_size 100,000:
  - `primes2.bas` - Only runs the prime sieve but shows no output
  - `count2.bas` - Runs the prime sieve and then shows the number of primes found and the expected number of primes

The actual sieve code in each `count` files is identical to the corresponding `primes` file. The former can be used to
run a "clean" benchmark of the solution. The latter can be used to validate the result of the sieve.

NOTE: 16 bits is the maximum number of bits that can be stored in an integer, so effectively, `primes2.bas` is a 1-bit
solution.

The one bit per array item solution was improved over davepl's original solution in the following ways:

- Only deal with odd numbers starting with 3. This reduces the array size from 10,000 to 4,999
- Start the inner loop at `factor**2` instead of `2*factor` and step by `2*factor` instead of `factor`
- `1` is used for composite, and `0` is used for prime, so there is no need to initialize the sieve since it is
  automatically initialized to `0`

## Run instructions

### Linux/cbmbasic

Execute the following commands from the implementation directory, in a bash shell:

```bash
./build.sh
./run.sh
```

### Docker

A Dockerfile has been provided. With Docker installed on a supported operating system, it can be used with the following commands:

```bash
docker build -t primes-basic-4 .
docker run primes-basic-4
```

## Output

On an Intel(R) Core(TM) i7-8700 CPU @ 3.20GHz with 32 GB of memory on a Windows 10 desktop running
a Ubuntu 22.04 VM in VirtualBox 6.1, the original solution by davepl had this result:

```log
davepl-rzuckerm-msbasic;402;5.011;1;algorithm=base,faithful=no
rzuckerm-msbasic-bit;8;5.368;1;algorithm=base,faithful=no,bits=1
```
