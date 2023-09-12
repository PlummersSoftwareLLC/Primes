# Unicat solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-sieve_size-blue)

## Introduction

Unicat is an esoteric language was developed by [gemdude46](https://github.com/gemdude46/unicat).
The language is entirely made of up 9 cat emojis. It has a very limited instruction set that can do
the following:

- Set the contents of a memory address to a constant (`mem[addr] = constant`).
- Do basic math (addition, subtraction, multiplication, and division) between a pair of memory
  addresses:
  - `mem[addr1] += mem[addr2]`
  - `mem[addr1] -= mem[addr2]`
  - `mem[addr1] *= mem[addr2]`
  - `mem[addr1] //= mem[addr2]` (integer division)
- Dereference a memory address (`mem[addr] = mem[mem[addr]]`).
- Output the contents of memory address as a number or a ASCII/Unicode character to `stdout`.
- Input an ASCII string from `stdin`, and store it to a consecutive set of memory addresses as
  the ASCII/Unicode value of each character with a null-termination.
- Conditionally jump to an instruction address if memory address contains a value greater than
  zero.
- Unconditionally jump to an insruction address.
- Exit the program.

The amount of memory is arbitrarily large, each memory address can hold an arbitrarily large
integer. Memory address `-1` is the instruction address. It is initialized to `-1`, and it
is incremented before the start of each instruction. Therefore, any jump (conditionally or
unconditionally) must be done to the desired instruction address minus 1. It should be noted that
instruction memory is totally separate from the memory used by the program, so the program cannot
modify itself. If you'd like a more in-depth description of the language, see
[this article](https://sampleprograms.io/languages/unicat/).

This is not intended as a serious solution. It was done more as a challenge to see if I could
actually implement something as complicated as a Prime Number Sieve in such a limited language and
have it complete in a reasonable amount of time. This solution is marked as "unfaithful" for the
following reasons:

- I could only get the solution to complete in a reasonable amount of time for 100,000 or less.
- Since Unicat has no ability to measure time, I had to write a python wrapper to run it
- I didn't want the compilation time to be counted, so
  [I wrote a version of the Unicat language](https://github.com/rzuckerm/unicat-esolang)
  based on [gemdude46's original code](https://github.com/gemdude46/unicat/blob/master/cat.py) in
  python 3 that separates the compilation from the execution (in addition to improving
  the performance of the language and fixing some bugs).

## Python implementation

At a high level, the steps are as follows:

- The Unicat code is compiled into a list of instructions
- While time limit not expired, the Unicat instructions are run, sending the sieve size to
  the `stdin` of the Unicat program, and capturing the `stdout` of the Unicat program, keeping
  track of the number of passes and the elapsed time
- The required information about the performance and accuracy of the Unicat program is displayed.
  The accuracy is determined by decoding the `stdout` of the Unicat program and comparing the number
  of primes found against the expected value for the sieve size

## Unicat implementation

At a high level, the steps are as follows:

- The sieve size (or limit) is taken from `stdin` and converted to an integer (`n`).
- The prime numbers up to `n` (for odd values starting at 3) are calculated, and the result
  (`sieve`) is stored as bitmap in a single memory address, where `1` means composite, and `0`
  means prime. The bits are numbered as follows:
  * Bit 0: `3`
  * Bit 1: `5`
  * ...
  * Bit `k`: `2*k + 3`
  * ...
  * Bit `(n - 3) // 2`: `n - (n mod 2)` (next lowest odd value -- e.g., 1000 becomes 999)
- The result (`sieve`) is output as a decimal value (plus a newline) that is decoded by the python
code.

Before diving into the actual implementation of the prime sieve, let's take a look at the algorithm
first:

```
sieve = 0
factor = 3
while factor*factor <= n:
  factor_bit = (factor - 3) // 2
  if bit "factor_bit" is not set in sieve:
    inner_factor = factor*factor
    while inner_factor <= n:
      inner_factor_bit = (inner_factor - 3) // 2
      Set bit "inner_factor_bit" in sieve
      inner_factor += 2*factor

  factor += 2

output sieve
output newline
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

The bit number calculations can be reworked as well using the above. However, everything is
halved since the bit number is calculated based on odd numbers.

Therefore, the algorithm can be refactored like this:

```
sieve = 0

factor = 3

factor_sq = 3*3 = 9
factor_sq_inc = 5*5 - 3*3 = 16

factor_bit = (3 - 3) // 2 = 0
factor_sq_bit = (9 - 3) // 2 = 3
factor_sq_bit_inc = 16 // 2 = 8

while factor_sq <= n:
  if bit "factor_bit" is not set in sieve:
    inner_factor = factor_sq
    inner_factor_bit = factor_sq_bit
    while inner_factor <= n:
      Set bit "inner_factor_bit" in sieve
      inner_factor += factor*2
      inner_factor_bit += factor

  factor += 2
  factor_bit += 1

  factor_sq += factor_sq_inc
  factor_sq_inc += 8

  factor_sq_bit += factor_sq_bit_inc
  factor_sq_bit_inc += 4

output sieve
output newline
```

Since Unicat does not have any bitwise operations, this must be simulated as follows:

- Testing if bit `x` is set in `y` is done by checking if `(y // 2**x) mod 2` is greater than zero,
  where `2**x` is pre-computed, `z mod 2` is calculated as `z - (z // 2) * 2`, and `z` is
  `y // 2**x`
- Setting bit `x` of `y` is done by adding `2**x` to `y` if bit `x` is not set in `y`.

Therefore, the algorithm needs to be reworked so that `2**bit_number` is pre-computed:

```
sieve = 0

factor_times_two = 3*2 = 6

factor_sq = 3*3 = 9
factor_sq_inc = 5*5 - 3*3 = 16

factor_mask = 2**[(3 - 3) // 2] = 2**0 = 1
factor_sq_mask = 2**[(9 - 3) // 2] = 2**3 = 8
factor_sq_mask_multiplier = 2**(16 // 2) = 2**8 = 256

inner_factor_mask_multiplier = 2**(6 // 2) = 2**3 = 8

while factor_sq <= n:
  temp = sieve // factor_mask
  if temp - (temp // 2) * 2 <= 0:
    inner_factor = factor_sq
    inner_factor_mask = factor_sq_mask
    while inner_factor <= n:
      temp = sieve // inner_factor_mask
      if temp - (temp // 2) * 2 > 0:
          sieve += inner_factor_mask

      inner_factor += factor_times_two
      inner_factor_mask *= inner_factor_mask_multiplier

  factor_times_two += 2*2 (4)
  factor_mask *= 2**1 (2)

  factor_sq += factor_sq_inc
  factor_sq_inc += 8

  inner_factor_mask_multiplier *= 2**2 (4)

  factor_sq_mask *= factor_sq_mask_multiplier
  factor_sq_mask_multiplier *= 2**4 (16)

output sieve
output newline
```

I later discovered that I could get a signifant performance gain by refactoring the
``sieve // inner_factor_mask`` out of the inner loop like this:

```
sieve = 0

factor_times_two = 3*2 = 6

factor_sq = 3*3 = 9
factor_sq_inc = 5*5 - 3*3 = 16

factor_mask = 2**[(3 - 3) // 2] = 2**0 = 1
factor_sq_mask = 2**[(9 - 3) // 2] = 2**3 = 8
factor_sq_mask_multiplier = 2**(16 // 2) = 2**8 = 256

inner_factor_mask_multiplier = 2**(6 // 2) = 2**3 = 8

while factor_sq <= n:
  temp = sieve // factor_mask
  if temp - (temp // 2) * 2 <= 0:
    inner_factor = factor_sq
    inner_factor_mask = factor_sq_mask
    inner_sieve = sieve // factor_sq_mask
    while inner_factor <= n:
      if inner_sieve - (inner_sieve // 2) * 2 <= 0:
          sieve += inner_factor_mask

      inner_factor += factor_times_two
      inner_factor_mask *= inner_factor_mask_multiplier
      inner_sieve //= inner_factor_mask_multiplier

  factor_times_two += 2*2 (4)
  factor_mask *= 2**1 (2)

  factor_sq += factor_sq_inc
  factor_sq_inc += 8

  inner_factor_mask_multiplier *= 2**2 (4)

  factor_sq_mask *= factor_sq_mask_multiplier
  factor_sq_mask_multiplier *= 2**4 (16)

output sieve
output newline
```

Since Unicat only has the ability to compare to zero, `x <= y` has to be reworked as `x - y <= 0`.
Therefore, the algorithm must be refactored to use an outer and inner counter like this:

```
outer_counter = 3*3 - n = 9 - n

sieve = 0

factor_times_two = 3*2 = 6

factor_sq = 3*3 = 9
factor_sq_inc = 5*5 - 3*3 = 16

factor_mask = 2**[(3 - 3) // 2] = 2**0 = 1
factor_sq_mask = 2**[(9 - 3) // 2] = 2**3 = 8
factor_sq_mask_multiplier = 2**(16 // 2) = 2**8 = 256

inner_factor_mask_multiplier = 2**(6 // 2) = 2**3 = 8

while outer_counter <= 0:
  temp = sieve // factor_mask
  if temp - (temp // 2) * 2 <= 0:
    inner_counter = outer_counter
    inner_factor_mask = factor_sq_mask
    inner_sieve = sieve // factor_sq_mask
    while inner_counter <= 0:
      if inner_sieve - (inner_sieve // 2) * 2 <= 0:
          sieve += inner_factor_mask

      inner_counter += factor_times_two
      inner_factor_mask *= inner_factor_mask_multiplier
      inner_sieve //= inner_factor_mask_multiplier

  factor_times_two += 2*2 (4)
  factor_mask *= 2**1 (2)

  outer_counter += factor_sq_inc
  factor_sq_inc += 8

  inner_factor_mask_multiplier *= 2**2 (4)

  factor_sq_mask *= factor_sq_mask_multiplier
  factor_sq_mask_multiplier *= 2**4 (16)

output sieve
output newline
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

- `-l <limit>` or `--limit <limit>` - Upper limit for calculating prime numbers. Default: 100000
- `-t <time>` or `--time <time>` - Time limit in seconds. Default: 5
- `-s` or `--show-results` - Print found prime numbers

## Output

On an Intel(R) Core(TM) i7-8700 CPU @ 3.20GHz with 32 GB of memory on a Windows 10 desktop running
a Ubuntu 22.04 VM in VirtualBox 6.1:

```console
$ ./run.sh -l 10
Passes: 54664, Time: 5.000086383999999, Avg: 10932.611119464213, Count: 4, Valid: True
rzuckerm;54664;5.000086383999999;1;algorithm=base,faithful=no,bits=1

$ ./run.sh -l 100
Passes: 10633, Time: 5.000036713, Avg: 2126.584385341492, Count: 25, Valid: True
rzuckerm;10633;5.000036713;1;algorithm=base,faithful=no,bits=1

$ ./run.sh -l 1000
Passes: 907, Time: 5.001584391999998, Avg: 181.34253646719242, Count: 168, Valid: True
rzuckerm;907;5.001584391999998;1;algorithm=base,faithful=no,bits=1

$ ./run.sh -l 10000
Passes: 66, Time: 5.026044870999996, Avg: 13.131597845617414, Count: 1229, Valid: True
rzuckerm;66;5.026044870999996;1;algorithm=base,faithful=no,bits=1

$ ./run.sh -l 100000
Passes: 3, Time: 6.704778101999992, Avg: 0.44744210089594455, Count: 9592, Valid: True
rzuckerm;3;6.704778101999992;1;algorithm=base,faithful=no,bits=1

$ ./run.sh -l 1000000
Passes: 1, Time: 245.23979778499998, Avg: 0.0040776415941946465, Count: 78498, Valid: True
rzuckerm;1;245.23979778499998;1;algorithm=base,faithful=no,bits=1
```
