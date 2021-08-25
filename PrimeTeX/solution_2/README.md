# TeX solution by jfbu

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)

The Sieve is implemented using only Knuth base TeX; the benchmarking however
requires the timing extensions of `pdftex` or `luatex` and also uses
`\dimexpr` and other e-TeX extensions as incorporated in `pdftex` and `luatex`
anyhow.

TeX has no native array type. The storage technique is via "font dimension
parameters": instantiating a Sieve object loads in TeX memory a font at a size
indexed on the instantiation number. The minimal non-zero font dimension will
serve to mark the non-primes (i.e. `1sp`, where `sp` is the "scaled point" TeX
dimensional unit, and it is stored as a `1` on a 32bits word), the `0sp`
signals primes: one full font dimension per number as there is no native TeX
interface to bitwise operations. Each font dimension occupies 32bits in
memory.

*faithfulness*: No, as the memory footprint is unavoidably global and can not
be released.  For the author amusement with TeX the user interface mimicks
some notions of objects and methods, but some of these pure TeX aspects
should be improved (as there is a mix of local and automatically global
assignments; also the `\newcount` by themselves means clashes with
any code using same names for `\count`).

Two algorithms are implemented:

- the "base" algorithm with sieving out starting at `factor * factor`
and going by steps of `2 * factor`,
- the "wheel-type" algorithms in three variants: i.e. with classes already
relatively prime to respectively `2*3*5`, `2*3*5*7` or `2*3*5*7*11`.

The longest wheel is implemented less efficiently than the other two as the
straightforward extension of the coding as used for the shorter wheels is
impeded by the limitation of TeX to at most 256 count registers. This is a bit
rhetorical as the `pdftex` binary incorporates extensions to TeX allowing more
such count registers, but I had decided from the start not to use extensions
of TeX original syntax.

`pdftex` has static memory, and even configuring it to its maximal TeXLive
memory setting, a maximum of `294` passes can be done with it for the sieving
range of `1,000,000`, as each pass consumes about `500,000` words of font
memory which can not be released (the memory could be re-used, but for
fairness of the benchmark each pass re-allocates new memory).

As at my locale I achieve with `pdftex` running in the Docker container about `80` passes for the
`wheel48of210` implementation, a computer about `3.7` times faster than mine
would exhaust the `pdftex` maximal font memory during the benchmark, even with
`pdftex` configured to use the maximal possibly font memory setting, as is
done via the `runpdftex.sh` script used by the Dockerfile.

The provided Dockerfile does nevertheless use `pdftex`. The alternative, as
applied in earlier versions of this TeX solution, is to benefit from
`luatex`'s dynamic memory allocation. But using this `luatex`'s feature also
revealed some problems of efficiency with it (which have been reported
upstream and are particularly visible on Windows platform), and gave runs
slightly slower than with `pdftex`.

For people with very fast hardware which will cause `pdftex` to try to do more
than `294` passes in `5s`, hence abort with a fatal error of exhausted memory,
a script to execute the benchmark with `luatex` rather is provided.


## Run instructions with Docker

```bash
docker build -t erato:latest .
docker run --rm erato
```

## Native runs

With `luatex`:

```bash
/bin/sh run.sh
```

With `pdftex` (as used by Dockerfile):

```bash
/bin/sh runpdftex.sh
```

If `295` or more passes are executed during the benchmark,
the `pdftex` run will fail due to exhausted memory.
Output will be empty.


## Output

Hardware: 2 GHz Intel Core i7 (one processor, two cores) with 8 Go 1600 MHz
DDR3 of memory (mid-2012 machine, native OS: mac osx high sierra).

Docker run with `pdftex`:

```
jfbu-tex;28;5.14409;1;algorithm=base,faithful=no,bits=32
jfbu-tex-8of30;73;5.0266;1;algorithm=wheel,faithful=no,bits=32
jfbu-tex-48of210;81;5.00916;1;algorithm=wheel,faithful=no,bits=32
jfbu-tex-480of2310;45;5.04994;1;algorithm=wheel,faithful=no,bits=32
```

Docker run (with a Dockerfile modified to use `luatex` via `run.sh` in place of `runpdftex.sh`):

```
jfbu-tex;26;5.0469;1;algorithm=base,faithful=no,bits=32
jfbu-tex-8of30;63;5.05162;1;algorithm=wheel,faithful=no,bits=32
jfbu-tex-48of210;71;5.06566;1;algorithm=wheel,faithful=no,bits=32
jfbu-tex-480of2310;41;5.12201;1;algorithm=wheel,faithful=no,bits=32
```

The reason why the 480-of-2310 wheel is significantly slower than the two
shorter wheels is due to the latter using optimized ways of making assignments
in a rolled-out loop. The straightforward extension of this to the 480-of-2310
wheel would need `480` count registers, but Knuth TeX has in total only `256`
at most.

Native runs:

- with `luatex` (`/bin/sh run.sh`)

  ```
  jfbu-tex;21;5.10782;1;algorithm=base,faithful=no,bits=32
  jfbu-tex-8of30;45;5.09566;1;algorithm=wheel,faithful=no,bits=32
  jfbu-tex-48of210;48;5.06216;1;algorithm=wheel,faithful=no,bits=32
  jfbu-tex-480of2310;31;5.00119;1;algorithm=wheel,faithful=no,bits=32
  ```

- with `pdftex` (`/bin/sh runpdftex.sh`)

  ```
  jfbu-tex;26;5.10503;1;algorithm=base,faithful=no,bits=32
  jfbu-tex-8of30;69;5.03265;1;algorithm=wheel,faithful=no,bits=32
  jfbu-tex-48of210;77;5.0467;1;algorithm=wheel,faithful=no,bits=32
  jfbu-tex-480of2310;43;5.02617;1;algorithm=wheel,faithful=no,bits=32
  ```

The native `luatex` is `1.13` from
TeXLive 2021.

The native `pdftex` is compiled
locally from sources with compiler flags for speed.

The Dockerfile is based on Ubuntu 20.04 and installs its
[texlive-base](https://packages.ubuntu.com/focal/texlive-base) package
(which uses TeXLive 2019).

I don't know why the speed ratio wheel/base is higher with `pdftex`, or with
the `luatex` in a Docker container, than with the native `luatex`.

## Information on some of the files

`texmf.cnf` is a file which instructs `pdftex` to use more memory, once the
`TEXMFCNF` environment variable is suitably set, as done by `runpdftex.sh`. It
also shows (commented-out) how to extend another part of `pdftex` memory,
which is not needed for sieving up to `1,000,000`, but would be needed for sieving
up to a bit less than `295,000,000` which is maximum compatible with maximal
font memory.

`erato_sieve.tex` is the core library providing the Sieve object and its
methods. It has `shared_batteries.tex` as dependency.

`wheel_sieve.tex` is the core library providing the Sieve object and its
methods. It also has `shared_batteries.tex` as dependency. It uses the
"480-out-of-2310" wheel.

`wheel8of30_sieve.tex`, `wheel48of210_sieve.tex`: as their names indicate.
They improve the execution speed via doing assignments in a "partially rolled
out loop" using suitably configured dedicated count registers.

`erato_primestofile.tex` is a file loading `erato_sieve.tex`, which you can run
with either Knuth base `tex` or `pdftex` or `luatex` to generate files with
one prime per line.  By default it generates `listofprimes-1000000.txt`.

`wheel_primestofile.tex` is a file loading `wheel_sieve.tex`, which you can run
with either Knuth base `tex` or `pdftex` or `luatex` to generate files with
one prime per line using the wheel algorithm.
By default it generates `listofprimes-1000000.txt`.

`wheel8of30_primestofile.tex`, `wheel48of210_primestofile.tex` are similar.

`{erato,wheel}_primestopdf_{h,v}.tex` are to be compiled with `pdflatex` or
`lualatex`.  They either use or re-compute `listofprimes-1000000.txt` (or some
other configurable range) and then output to `pdf` the prime numbers in a
column-wise manner, the columns being filled from left to right for the `h`
version and from top to bottom for the `v` version.

`timelistofprimes.sh` is to be executed with `/bin/sh` and an additional
argument which must be one of `erato`, `wheel`, `wheel8of30`, or
`wheel48of210`:

```bash
/bin/sh timelistofprimes.sh erato
/bin/sh timelistofprimes.sh wheel8of30
/bin/sh timelistofprimes.sh wheel48of210
/bin/sh timelistofprimes.sh wheel
```

These scripts test the production of files `listofprimes-<range>.txt` for
`<range>` varying from `1,000,000` to `999,999,999` via the chosen algorithm.
Regarding `pdftex` they require a relatively recent version as it must
understand the `-cnf-line` option.

The costliest part is (by far) not the sieving but the creation of the text
files, one prime per line; I have not tried seriously to boost this as it is
not topic of the drag-race.

For example (notice the command line options to enlarge `pdftex` static memory):

```
time pdftex -cnf-line font_mem_size=51000000 -cnf-line extra_mem_top=10000000 "\def\Range{100000000}\input wheel48of210_primestofile"
```

sieves up to `100,000,000` in about `7s` at my locale, but then needs
an additional `18s` to produce the file with the `5,761,455` primes
`<100,000,000`, one per line.

The above syntax (without the `time`) works also on Windows with TeXLive. On Windows with MikTeX, try this:

```
pdftex.exe -font-mem-size=51000000 -extra-mem-top=13000000 "\def\Range{100000000}\input wheel48of210_primestofile"
```


Even with maximal font memory, `pdftex` will not allow sieving up to about `295,000,000` and beyond. With `luatex` we can go up to `999,999,999`:

```
time luatex "\def\Range{999999999}\input wheel48of210_primestofile"
```

Again, it takes more time (about four minutes at my locale) to create the file
with the `50,847,534` primes `<999,999,999` than to actually sieve the prime
array (about two minutes).

See files `{erato,wheel8of30,wheel48of210,wheel}_primestofile_timings.txt` for
some further typical timings as seen on my hardware. These files contain the
console commands needed to execute the various runs in a syntax which I am
told is Windows-compatible (if using TeXLive), apart from the usage of `time` utility.


## More info on native runs with pdftex

Executing

```
pdftex erato_benchmark
```

will certainly fail as `pdftex`'s font memory is per default only
of `8,000,000` words, and each pass consumes `500,000` words, so at most
`15` passes can be done before exhausting the memory. And with my
relatively slow hardware, already more than `25` passes are done.

To enlarge the `pdftex` static memory, make sure that the repertory contains
the contributed file `texmf.cnf` and do `export TEXMFCNF="$(pwd):"` and then
the run should succeed.

Or, with a recent `pdftex` one can use the `-cnf-line` option rather than lines in `texmf.cnf`.

As a further shortcut, one can use `/bin/sh runpdftex.sh` rather which avoids
leaving `TEXMFCNF` set in the environment (which `context` does not like).

See `texmf.cnf` for additional information.
