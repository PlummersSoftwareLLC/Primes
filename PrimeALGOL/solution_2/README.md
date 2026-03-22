# ALGOL 60 solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

* `primes.a60` uses `boolean` for each sieve item

ALGOL 60 has some annoying limitations:

- It has no command-line interface, so command-line arguments are handled though
  `run-primes.sh` and piped to stdin
- It has no dynamic memory allocation, so all memory allocation is only done once.
- It has no system timer. However, since
  [GNU MARST](https://ftp.gnu.org/gnu/marst/marst-2.8.tar.gz) is used to translate
  ALGOL 60 to C, C code can be embedded using the `inline` function
- Output of numeric values always have a space after them, so `run-primes.sh`
  has to remove that from the output using `sed`

## Run instructions

Build the docker image with this:

```bash
./build.sh
```

You should only need to do this once. Run the docker image:

```bash
./run.sh [<args>]
```

where `<args>` are optional command-line arguments:
- `--limit/-l <limit>` - Upper limit for calculating prime numbers. Default: 1000000
- `--time/-t <time>` - Time limit in seconds. Default: 5
- `--show-results/-s` - Print found prime numbers

## Output

On a 12th Gen Intel(R) Core(TM) i7-1255U 1.70 GHz with 16 GB of memory on a Windows 11
laptop running Ubuntu 22.04 in WSL2:

```
Passes: 5644434 Time: 5 Avg: 8.85828410785e-07 Limit: 10 Count1: 4 Count2: 4 Valid: true
rzuckerm-algol60;5644434;5;1;algorithm=base,faithful=no

Passes: 1479489 Time: 5 Avg: 3.37954523488e-06 Limit: 100 Count1: 25 Count2: 25 Valid: true
rzuckerm-algol60;1479489;5;1;algorithm=base,faithful=no

Passes: 174405 Time: 5.000004 Avg: 2.86689257762e-05 Limit: 1000 Count1: 168 Count2: 168 Valid: true
rzuckerm-algol60;174405;5.000004;1;algorithm=base,faithful=no

Passes: 22154 Time: 5.000011 Avg: 0.000225693373657 Limit: 10000 Count1: 1229 Count2: 1229 Valid: true
rzuckerm-algol60;22154;5.000011;1;algorithm=base,faithful=no

Passes: 2258 Time: 5.001467 Avg: 0.00221499867139 Limit: 100000 Count1: 9592 Count2: 9592 Valid: true
rzuckerm-algol60;2258;5.001467;1;algorithm=base,faithful=no

Passes: 197 Time: 5.014032 Avg: 0.0254519390863 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
rzuckerm-algol60;197;5.014032;1;algorithm=base,faithful=no

Passes: 11 Time: 5.308077 Avg: 0.482552454545 Limit: 10000000 Count1: 664579 Count2: 664579 Valid: true
rzuckerm-algol60;11;5.308077;1;algorithm=base,faithful=no
```
