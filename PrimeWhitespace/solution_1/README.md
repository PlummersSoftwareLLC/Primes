# Whitespace solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-sieve_size-blue)

## Introduction

Whitespace is an esoteric language was developed by Edwin Brady and Chris Morris.
The language is entirely made up of 3 whitespace character: space, tab, and newline.
Every other character is ignored. It is customary to make the code a little more
"readable" by marking each character with `S` for space, `T` for tab, and `N` for newline.
It has a very limited instruction set. See
[this Wikipedia article](https://en.wikipedia.org/wiki/Whitespace_%28programming_language%29)
for details.

Since Whitespace is such a difficult language to work with, I wrote an
[assembler](https://github.com/rzuckerm/whitespace-asm) to generate the Whitespace code from
my own version of Whitespace Assembly language. The
[whitespace-primes](https://github.com/rzuckerm/whitespace-primes/tree/main) repository
has a [template](https://github.com/rzuckerm/whitespace-primes/blob/main/template/primes.ws.asm.j2)
that I used to generate the assembly code. The generated assembly code has a `.ws.asm` extension
and is in the [whitespace](https://github.com/rzuckerm/whitespace-primes/tree/main/whitespace)
directory.

This is not intended as a serious solution. It was done more as a challenge to see if I could
actually implement something as complicated as a Prime Number Sieve in such a limited language and
have it complete in a reasonable amount of time. This solution is marked as "unfaithful" for the
following reasons:

- I could only get the solution to complete in a reasonable amount of time for 100,000 or less.
- Since Whitespace has no ability to measure time, I had to write a python wrapper to run it.

## Python implementation

At a high level, the steps are as follows:

- Parse the command-line arguments
- While time limit not expired, the Whitespace program for the appropriate number of bits per
- word is run, sending the sieve size to the `stdin` and capturing the `stdout`, keeping track
  of the number of passes and the elapsed time
- The required information about the performance and accuracy of the Whitespace program is displayed.
  The accuracy is determined by decoding the `stdout` of the Whitespace program and comparing the number
  of primes found against the expected value for the sieve size

## Whitespace implementation

At a high level, the steps are as follows:

- The sieve size (or limit) is taken from `stdin` and converted to an integer (`n`).
- The prime numbers up to `n` (for odd values starting at 3) are calculated, and the result
  (`sieve`) is stored as bitmap in a set of memory addresses (the size of which is determined
  by the number of bits per word), where `1` means composite, and `0`
  means prime. The bits are numbered as follows:
  * Bit 0: `3`
  * Bit 1: `5`
  * ...
  * Bit `k`: `2*k + 3`
  * ...
  * Bit `(n - 3) // 2`: `n - (n mod 2)` (next lowest odd value -- e.g., 1000 becomes 999)
- The result (`sieve`) is output as a set space-separated decimal values that is decoded by the
  python code.

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

output sieve as space-separated decimal value
```

The actual implementation uses bit numbers instead of factors:

```
sieve = 0
b = 0
bsq = 3
while bsq < B:
  if bit b clear in sieve:
    k = bsq
    kinc = 2 * b + 3
    while k < B:
      Set bit k in sieve
      k += kinc

    b += 1
    bsq += 4 * (b + 1)

output sieve as space-separated decimal value
```

where:

- `b` is the bit number for the factor
- `bsq` is the bit number for the factor squared
- `B` is the total number of bits = `(n - 1) // 2`
- `k` is the bit number of the inner factor
- `kinc` is the increment for the inner factor

Since the sieve bits start at a factor of 3 and only have odd values, the bit number can be
determined like this:

```
bit_number = (factor - 3) // 2
```

Therefore, the initial value for `b` and `bsq` are determined as follows:

```
b = (3 - 3) // 2 = 0
bsq = (3*3 - 3) // 2 = 6 // 2 = 3
```

The value of `k` starts out as `bsq` (which corresponds to `factor**2`). The increment can be
determined by the difference between consecutive factors (`2*factor`):

```
factor = 2*b + 3
f1 = m*factor = m*(2*b + 3)
k1 = (f1 - 3) // 2 = [m*(2*b + 3) - 3] // 2
f2 = (m + 2)*factor = (m + 2)*(2*b + 3)
k2 = (f2 - 3) // 2 = [(m + 2)*(2*b + 3) - 3] // 2
kinc = k2 - k1 = {[(m + 2)*(2*b + 3) - 3] - [m*(2*b + 3) - 3]} // 2
     = 2*(2*b + 3) // 2 = 2*b + 3
```

The difference between consecutive `factor**2` values can be determined as follows:

```
factor_sq_diff = (factor + 2)**2 - factor**2 = 4*factor + 4
bsq_diff = 4*(2*b + 3 - 3) // 2 + 4 = 4*b + 4 = 4*(b + 1)
```

Since Whitespace does not have any bitwise operations, this must be simulated as follows:

- Testing if bit `x` is set in `y` is done by checking if `(y // 2**x) mod 2` is greater than zero,
  where `2**x` is pre-computed
- Setting bit `x` of `y` is done by adding `2**x` to `y` if bit `x` is not set in `y`.

Since the sieve is broken up into a number of W-bit words, the bitwise operations are
done on the following:

```
index = x // W
mask = 2**(x mod W)
```

where:

- `index` is the index into the sieve words
- `mask` is the pre-computed values of `2**m` for `m` is `0` to `W-1`

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

- `-b` or `--bits-per-word` - The number of bits per word. Default: 32
- `-l <limit>` or `--limit <limit>` - Upper limit for calculating prime numbers. Default: 100000
- `-t <time>` or `--time <time>` - Time limit in seconds. Default: 5
- `-s` or `--show-results` - Print found prime numbers

## Output

On a 12th Gen Intel(R) Core(TM) i7-1255U 1.70 GHz with 16 GB of memory on a Windows 11
laptop running Ubuntu 22.04 in WSL2:

```console
$ ./run.sh -l 10

Passes: 405, Time: 5.006196131002071, Avg: 80.89974691401727, Count: 4, Valid: True
rzuckerm-whitespace-32bit;405;5.006196131002071;1;algorithm=base,faithful=no,bits=1

Passes: 402, Time: 5.002014746998611, Avg: 80.36761591740897, Count: 4, Valid: True
rzuckerm-whitespace-64bit;402;5.002014746998611;1;algorithm=base,faithful=no,bits=1

$ ./run.sh -l 100

Passes: 404, Time: 5.003481171999738, Avg: 80.74378340041471, Count: 25, Valid: True
rzuckerm-whitespace-32bit;404;5.003481171999738;1;algorithm=base,faithful=no,bits=1

Passes: 403, Time: 5.001387503001752, Avg: 80.57763965662048, Count: 25, Valid: True
rzuckerm-whitespace-64bit;403;5.001387503001752;1;algorithm=base,faithful=no,bits=1

$ ./run.sh -l 1000

Passes: 404, Time: 5.010348473999329, Avg: 80.63311406312656, Count: 168, Valid: True
rzuckerm-whitespace-32bit;404;5.010348473999329;1;algorithm=base,faithful=no,bits=1

Passes: 413, Time: 5.004141896999499, Avg: 82.53163249580037, Count: 168, Valid: True
rzuckerm-whitespace-64bit;413;5.004141896999499;1;algorithm=base,faithful=no,bits=1

$ ./run.sh -l 10000

Passes: 59, Time: 5.025010895999003, Avg: 11.741268073065628, Count: 1229, Valid: True
rzuckerm-whitespace-32bit;59;5.025010895999003;1;algorithm=base,faithful=no,bits=1

Passes: 60, Time: 5.0060970800004725, Avg: 11.985384829971043, Count: 1229, Valid: True
rzuckerm-whitespace-64bit;60;5.0060970800004725;1;algorithm=base,faithful=no,bits=1

$ ./run.sh -l 100000

Passes: 5, Time: 5.6302370939993125, Avg: 0.8880620685279813, Count: 9592, Valid: True
rzuckerm-whitespace-32bit;5;5.6302370939993125;1;algorithm=base,faithful=no,bits=1

Passes: 5, Time: 5.030771072000789, Avg: 0.9938834282935178, Count: 9592, Valid: True
rzuckerm-whitespace-64bit;5;5.030771072000789;1;algorithm=base,faithful=no,bits=1

$ ./run.sh -l 1000000

Passes: 1, Time: 50.763443834999634, Avg: 0.019699215113347664, Count: 78498, Valid: True
rzuckerm-whitespace-32bit;1;50.763443834999634;1;algorithm=base,faithful=no,bits=1

Passes: 1, Time: 29.51295403199765, Avg: 0.033883426203822564, Count: 78498, Valid: True
rzuckerm-whitespace-64bit;1;29.51295403199765;1;algorithm=base,faithful=no,bits=1
```

This result is rather unexpected since sieve sizes from 10 through 1000 have the same number
of passes. This indicates that the bottleneck is probably the overhead of spinning up
a subprocess for the Whitespace interpreter. Also, 64-bit words do not seems to have
a performance advantage until a sieve size of 100000.
