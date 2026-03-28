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
Passes: 11246467 Time: 5 Avg: 4.445840636e-07 Limit: 10 Count1: 4 Count2: 4 Valid: true
rzuckerm-algol60-bool;11246467;5;1;algorithm=base,faithful=no

Passes: 5499644 Time: 5 Avg: 9.09149755875e-07 Limit: 100 Count1: 25 Count2: 25 Valid: true
rzuckerm-algol60-bool;5499644;5;1;algorithm=base,faithful=no

Passes: 916891 Time: 5.000003 Avg: 5.45321417704e-06 Limit: 1000 Count1: 168 Count2: 168 Valid: true
rzuckerm-algol60-bool;916891;5.000003;1;algorithm=base,faithful=no

Passes: 90992 Time: 5.000007 Avg: 5.49499626341e-05 Limit: 10000 Count1: 1229 Count2: 1229 Valid: true
rzuckerm-algol60-bool;90992;5.000007;1;algorithm=base,faithful=no

Passes: 6499 Time: 5.000693 Avg: 0.000769455762425 Limit: 100000 Count1: 9592 Count2: 9592 Valid: true
rzuckerm-algol60-bool;6499;5.000693;1;algorithm=base,faithful=no

Passes: 593 Time: 5.004047 Avg: 0.00843852782462 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
rzuckerm-algol60-bool;593;5.004047;1;algorithm=base,faithful=no

Passes: 46 Time: 5.051831 Avg: 0.109822413043 Limit: 10000000 Count1: 664579 Count2: 664579 Valid: true
rzuckerm-algol60-bool;46;5.051831;1;algorithm=base,faithful=no

Passes: 4 Time: 5.146225 Avg: 1.28655625 Limit: 100000000 Count1: 5761455 Count2: 5761455 Valid: true
rzuckerm-algol60-bool;4;5.146225;1;algorithm=base,faithful=no

Passes: 1 Time: 14.276457 Avg: 14.276457 Limit: 1000000000 Count1: 50847534 Count2: 50847534 Valid: true
rzuckerm-algol60-bool;1;14.276457;1;algorithm=base,faithful=no
```
