# ALGOL 60 solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

* `primes.a60` uses `boolean` for each sieve item

ALGOL 60 has some annoying limitations:

- It has no command-line interface, so command-line arguments are handled though
  `run-primes.sh` and piped to stdin
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
Passes: 11883512 Time: 5.000015 Avg: 4.20752299489e-07 Limit: 10 Count1: 4 Count2: 4 Valid: true
rzuckerm-algol60-bool;11883512;5.000015;1;algorithm=base,faithful=no

Passes: 5518413 Time: 5 Avg: 9.06057593007e-07 Limit: 100 Count1: 25 Count2: 25 Valid: true
rzuckerm-algol60-bool;5518413;5;1;algorithm=base,faithful=no

Passes: 972844 Time: 5.000001 Avg: 5.13957119538e-06 Limit: 1000 Count1: 168 Count2: 168 Valid: true
rzuckerm-algol60-bool;972844;5.000001;1;algorithm=base,faithful=no

Passes: 95601 Time: 5.000051 Avg: 5.23012416188e-05 Limit: 10000 Count1: 1229 Count2: 1229 Valid: true
rzuckerm-algol60-bool;95601;5.000051;1;algorithm=base,faithful=no

Passes: 7003 Time: 5.000661 Avg: 0.000714074111095 Limit: 100000 Count1: 9592 Count2: 9592 Valid: true
rzuckerm-algol60-bool;7003;5.000661;1;algorithm=base,faithful=no

Passes: 568 Time: 5.007156 Avg: 0.00881541549296 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
rzuckerm-algol60-bool;568;5.007156;1;algorithm=base,faithful=no

Passes: 45 Time: 5.011531 Avg: 0.111367355556 Limit: 10000000 Count1: 664579 Count2: 664579 Valid: true
rzuckerm-algol60-bool;45;5.011531;1;algorithm=base,faithful=no

Passes: 5 Time: 6.014875 Avg: 1.202975 Limit: 100000000 Count1: 5761455 Count2: 5761455 Valid: true
rzuckerm-algol60-bool;5;6.014875;1;algorithm=base,faithful=no

Passes: 1 Time: 13.957539 Avg: 13.957539 Limit: 1000000000 Count1: 50847534 Count2: 50847534 Valid: true
rzuckerm-algol60-bool;1;13.957539;1;algorithm=base,faithful=no
```
