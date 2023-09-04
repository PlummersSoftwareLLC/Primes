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
  * Bit `(n - 3) // 2`: `n`
* The result (`sieve`) is output as a decimal value that is decoded by the python code.

Before diving into the actual implementation of the prime sieve, let's take a look at the algorithm
first:

```
sieve = 0
factor = 3
while factor*factor <= n:
  factor_bit = (factor - 3) // 2
  if bit "factor_bit" is not set in sieve:
    k = factor*factor
    while k <= n:
      k_bit = (k - 3) // 2
      Set bit "k_bit" in sieve
      k += 2*factor

    factor += 2

output sieve
```

Since bit number is actually what is important, this can be reworked to just deal this bit number.

```
num_bits = (n - 3) // 2
factor = 3
factor_bit = (3 - 3) // 2 = 0
while factor*factor <= n:
  if bit "factor_bit" is not set in sieve:
    k_bit = (factor*factor - 3) // 2
    while k_bit <= num_bits:
      Set "k_bit" in sieve
      k_bit += factor

  factor += 2
  factor_bit += 1

output sieve
```

The square operation can be reworked as repeat addition. Using the fact that the factor is
increased by 2 each time, an increment that must be added to the current value of `factor*factor`
to get the next value is this:

```
factor_sq_inc = (factor + 2)**2 - factor**2 = 4*factor + 4
```

The difference between this increment value and the next can be calculated as this:

```
4*(factor + 2) - 4*factor = 4*2 = 8
```

Therefore, `factor*factor` can be calculated like this:

```
initial factor_sq = 3*3 = 9
initial factor_sq_inc = 5*5 - 3*3 = 25 - 9 = 16
next factor_sq = current factor_sq + current factor_sq_inc
next factor_sq_inc = current factor_sq_inc + 8
```

However, we're interested in bit numbers, so this changes as follows:

```
initial factor_sq_bit = (3*3 - 3) // 2 = 3
initial factor_sq_inc_bit = 16 // 2 = 8
next factor_sq_bit = current factor_sq_bit + factor_sq_inc_bit
next factor_sq_inc_bit = current factor_sq_inc_bit + 4
```

Here's the algorithm is transformed to just work with bit numbers:

```
num_bits = (n - 3) // 2
factor = 3
factor_bit = (3 - 3) // 2 = 0
factor_sq_bit = (3*3 - 3) // 2 = 3
factor_sq_bit_inc = (5*5 - 3*3) // 2 = 8
while factor_sq_bit <= num_bits:
  if bit "factor_bit" is not set in sieve:
    k_bit = factor_sq_bit
    while k_bit <= num_bits:
      Set "k_bit" in sieve
      k_bit += factor

  factor += 2
  factor_bit += 1
  factor_sq_bit += factor_sq_bit_inc
  factor_sq_bit_inc += 4

output sieve
```

Since Unicat does not have any bitwise operations, this must be simulated as follows:

* Testing if bit `x` is set in `y` is done by checking if `(y // 2**x) mod 2` is greater than zero,
  where `2**x` is pre-computed, `z mod 2` is calculated as `z - (z // 2) * 2`, and `z` is
  `y // 2**x`
* Setting bit `x` of `y` is done by adding `2**x` to `y` if bit `x` is not set in `y`.

Therefore, the algorithm needs to be transformed to use this simulated behavior:

```
num_bits = (n - 3) // 2
factor_mask = 2**[(3 - 3) // 2]) = 2**0 = 1
factor_mask_multiplier = 2**3 = 8

factor_sq_bit = (3*3 - 3) // 2 = 3
factor_sq_bit_inc = (5*5 - 3*3) // 2 = 8
factor_sq_mask = 2**factor_sq_bit = 2**3 = 8
factor_sq_mask_multiplier = 2**factor_sq_bit_inc = 2**8 = 256

while factor_sq_bit <= num_bits:
  temp = sieve // factor_mask
  if temp - (temp // 2) * 2 == 0:
    k_bit = factor_sq_bit
    k_mask = factor_sq_mask
    while k_bit <= num_bits:
      temp = sieve // k_mask
      if temp - (temp // 2) * 2 == 0:
        sieve += k_mask

      k_bit += factor
      k_mask *= factor_mask_multiplier

  factor_mask *= 2
  factor_mask_multiplier *= 2

  factor_sq_bit += factor_sq_bit_inc
  factor_sq_bit_inc += 4

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
Passes: 51087, Time: 5.000006045000191, Avg: 10217.387647177944, Count: 4, Valid: True
rzuckerm;51087;5.000006045000191;1;algorithm=base,faithful=no,bits=1

Passes: 8221, Time: 5.000473509000585, Avg: 1644.0443060447453, Count: 25, Valid: True
rzuckerm;8221;5.000473509000585;1;algorithm=base,faithful=no,bits=1

Passes: 663, Time: 5.001794854000764, Avg: 132.55241755260897, Count: 168, Valid: True
rzuckerm;663;5.001794854000764;1;algorithm=base,faithful=no,bits=1

Passes: 39, Time: 5.028150220998214, Avg: 7.756331510767298, Count: 1229, Valid: True
rzuckerm;39;5.028150220998214;1;algorithm=base,faithful=no,bits=1

Passes: 1, Time: 30.191323400998954, Avg: 0.03312209891292518, Count: 9592, Valid: True
rzuckerm;1;30.191323400998954;1;algorithm=base,faithful=no,bits=1
```
