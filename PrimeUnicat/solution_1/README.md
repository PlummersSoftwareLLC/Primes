# Unicat solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Introduction

Unicat is an esoteric language was developed by [gemdude46](https://github.com/gemdude46/unicat).
The language is entirely made of up 9 cat emojis. It has a very limited instruction set that can do
the following:

* Set the contents of a memory address to a constant (`mem[addr] = constant`).
* Do basic math (addition, subtraction, multiplication, and division) between a pair of memory
  addresses:
  * `mem[addr1] += mem[addr2]`
  * `mem[addr1] -= mem[addr2]`
  * `mem[addr1] *= mem[addr2]`
  * `mem[addr1] //= mem[addr2]` (integer division)
* Dereference a memory address (`mem[addr] = mem[mem[addr]]`.
* Output the contents of memory address as a number or a ASCII/Unicode character to `stdout`.
* Input an ASCII string from `stdin`, and store it to a consecutive set of memory addresses as
  the ASCII/Unicode value of each character with a null-termination.
* Conditional jump to an instruction address if memory address contains a value greater than zero.
* Unconditionally jump to an insruction address.
* Exit the program.

The amount of memory is arbitrarily large, each memory address can hold an arbitrarily large
integer. If you'd like a more in-depth description of the language, see
[this article](https://sampleprograms.io/languages/unicat/).

This is not intended as a serious solution. It was done more as a challenge to so if I could actually
implement something as complicated as a Prime Number Sieve in such a limited language and have
it complete in a reasonable amount of time. This solution is marked as "unfaithful" for the following
reasons:

* I could only get the solution to complete in a reasonable amount of time for 100,000 or less.
* Since Unicat has no ability to measure time, I had to write a python wrapper to run it
* I didn't want the compilation time to be counted, so
  [I wrote a version of the Unicat language](https://github.com/rzuckerm/unicat-esolang)
  based on [gemdude46's original code](https://github.com/gemdude46/unicat/blob/master/cat.py) in
  python 3 that separates the compilation from the execution.

## Python implementation

At a high level, the steps are as follows:

* The Unicat code is compiled into a list of instructions
* While time limit not expired, the Unicat instructions are run, sending the sieve size to
  the `stdin` of the Unicat program, and capturing the `stdout` of the Unicat program, keeping
  track of the number of passes and the elapsed time
* The required information about the performance and accuracy of the Unicat program is displayed.
  The accuracy is determined by decoding the `stdout` of the Unicat program and comparing the number
  of primes found against the expected value for the sieve size

## Unicat implementation

At a high level, the steps are as follows:

* The sieve size (or limit) is taken from `stdin` and converted to an integer (`n`).
* The prime numbers up to `n` (for odd values starting at 3) is calculated, and the result
  (`sieve`) is stored as bitmap in a single memory address, where `1` means composite, and `0`
  means prime. The bits are numbered as follows:
  * Bit 0: `3`
  * Bit 1: `5`
  * ...
  * Bit `k`: `2*k + 3`
  * ...
* The result (`sieve`) is output as a decimal value that is decoded by the python code.

Since Unicat does not have any bitwise operations, this must be simulated as follows:

* Testing if bit `x` is set in `y` is done by checking if `(y // 2**x) mod 2` is greater than zero,
  where `2**x` is pre-computed.
* Setting bit `x` of `y` is done by adding `2**x` to `y` if bit `x` is not set in `y`.

Before diving into the actual implementation, let's take a look at the algorithm first:

```
sieve = 0
factor = 3
while factor*factor <= n:
    factor_bit = (factor - 3) // 2
    if bit "factor_bit" is not set in sieve:
        for k = factor*factor to n step factor*2:
            k_bit = (k - 3) / 2
            if bit "k_bit" is not set in sieve:
                sieve += 2**k_bit

    factor += 2

output sieve
```

## Run instructions

Build the docker image with this:

```bash
./build.sh
```

You should only need to do this once. Run the docker image:

```bash
./run.sh
```

## Command-line arguments

You can add the following command-line arguments to `run.sh`:

* `-l <limit>` or `--limit <limit>` - Upper limit for calculating prime numbers. Default: 100000
* `-t <time>` or `--time <time>` - Time limit in seconds. Default: 5
* `-s` or `--show-results` - Print found prime numbers

## Output

On an Intel(R) Core(TM) i7-8700 CPU @ 3.20GHz with 32 GB of memory on a Windows 10 desktop running
a Ubuntu 22.04 VM in VirtualBox 6.1:

```
Passes: 51853, Time: 5.000091032000029, Avg: 10370.411192145611, Count: 4, Valid: True
rzuckerm;51853;5.000091032000029;1;algorithm=base,faithful=no,bits=1

Passes: 8385, Time: 5.0001593920000005, Avg: 1676.9465416273672, Count: 25, Valid: True
rzuckerm;8385;5.0001593920000005;1;algorithm=base,faithful=no,bits=1

Passes: 660, Time: 5.006048645999954, Avg: 131.84050868689982, Count: 168, Valid: True
rzuckerm;660;5.006048645999954;1;algorithm=base,faithful=no,bits=1

Passes: 39, Time: 5.016214545000025, Avg: 7.774787073027755, Count: 1229, Valid: True
rzuckerm;39;5.016214545000025;1;algorithm=base,faithful=no,bits=1

Passes: 1, Time: 29.761020798000004, Avg: 0.03360099798953139, Count: 9592, Valid: True
rzuckerm;1;29.761020798000004;1;algorithm=base,faithful=no,bits=1
```
