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

TeX has no native array type.  The storage technique is via "font dimension
parameters": instantiating a Sieve object loads in TeX memory a font at a size
indexed on the instantiation number.  The minimal non-zero font dimension (`1sp`, stored as a `1` on a 32bits word) will serve to mark
the non-primes, the `0sp` signals primes: one full font dimension per number as there is no native TeX
interface to bitwise operations.  Each font dimension occupies 32bits in memory.

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
straightforward extension of the coding is impeded by the TeX maximal number
of 255 count registers.

The Dockerfile is configured to run the benchmark using `luatex`, as it has
dynamic memory allocation. In contrast `pdftex` has static memory, and even
configuring it to its maximal TeXLive memory setting, a maximum of `294`
passes can be done with it for the sieving range of `1,000,000`, as each pass
consumes about `500,000` words of font memory which can not be released (the
memory could be re-used, but for fairness of the benchmark each pass re-allocates new
memory).

As at my locale I achieve with `pdftex` about `77` passes for the `wheel48of210`
implementation, so a computer about `4` times faster than mine would exhaust
the `pdftex` maximal font memory if the benchmark was done with `pdftex`, even
configured to use the maximal possibly memory setting.


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

With `pdftex`:

```bash
/bin/sh runpdftex.sh
```

If `295` or more passes are executed during the benchmark,
the `pdftex` run will fail due to exhausted memory.
`pdftex` has no dynamic memory management. Output will be empty.


## Output

Hardware: 2 GHz Intel Core i7 (one processor, two cores) with 8 Go 1600 MHz
DDR3 of memory (mid-2012 machine, native OS: max osx high sierra).

Docker run:

```
jfbu-tex;22;5.02206;1;algorithm=base,faithful=no,bits=32
jfbu-tex-8of30;55;5.0123;1;algorithm=wheel,faithful=no,bits=32
jfbu-tex-48of210;60;5.03543;1;algorithm=wheel,faithful=no,bits=32
jfbu-tex-480of2310;35;5.09851;1;algorithm=wheel,faithful=no,bits=32
```

The reason why the 480-of-2310 wheel is significantly slower than the two
shorter wheels is due to the latter using optimized ways of making assignments
in a rolled-out loop. The straightforward extension of this to the 480-of-2310
wheel would need `480` count registers, but Knuth TeX has in total only `255`
at most.

As the benchmark is actually done using the `luatex` binary, we could obtain
a significant speed-up of the 480-of-2310 wheel implementation if we
actually dropped the requirement of a pure Knuth TeX solution and
extended the techniques as already applied to the 8-of-30 and 48-of-210 cases.

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
TeXLive 2021, the one of the Dockerfile is `1.07.0` from TeXLive 2018.
The native `pdftex` is compiled
locally from sources with compiler flags for speed.

I don't know why the speed ratio wheel/base is higher with `pdftex`, or with
the `luatex` from the Docker run, than with native `luatex`. It may be that
the size of the texmf trees impacts the speed of the `luatex` memory access
(?? only an uninformed guess).

## Information on some of the files

`Dockerfile` is based on
[`texlive-minimal`](https://hub.docker.com/r/phipsgabler/texlive-minimal) from
phipsgabler and uses a TeXLive 2018 installation.  In case you wan't to re-use it and expand it with addition of more TeXLive 2018 contents, this can be useful:

```
RUN tlmgr update --repository http://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2018/tlnet-final/ --self
RUN tlmgr install --repository http://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2018/tlnet-final/ <whatever>
```

I wanted to do this with `pdftexcmds.sty` to have `\pdfresettimer` with
`luatex` but this turned out to be complicated as this meant installing the
whole `oberdiek` bundle and then I encountered a problem, having spent enough
time on this I finally copied over directly the relevant lua code for timing
into the benchmark TeX files, rather than experiment with Docker TeXLive
difficulties.

`texmf.cnf` is a file which instructs `pdftex` to use more memory, once
the `TEXMFCNF` environment variable is suitably set, as done by `runpdftex.sh`.

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
`wheel48of210` (no error check is done):

```bash
/bin/sh timelistofprimes.sh erato
/bin/sh timelistofprimes.sh wheel8of30
/bin/sh timelistofprimes.sh wheel48of210
/bin/sh timelistofprimes.sh wheel
```

These scripts test the production of files `listofprimes-<range>.txt` for
`<range>` varying from `1,000,000` to `999,999,999` via the chosen algorithm.
The costliest part is (by far) not the sieving but the creation of the text
files, one prime per line; I have not tried seriously to boost this as it is
not topic of the drag-race. See files
`{erato,wheel,wheel8of30,wheel48of210,wheel}_primestofile_timings.txt` for the typical timings
seen on my hardware.

For example, the `wheel8of30` sieve will sieve up to `999,999,999` with luatex
in about `2mn20s` at my locale (and it would probably be faster in a Docker
container with less many files in `texmf` trees), but it then takes an
additional `3mn50s` to produce the file with the `50,847,534` primes, one per
line.


## More info on native runs with pdftex

Executing

```
pdftex erato_benchmark && cat erato_benchmark-out.txt
```

will certainly fail as `pdftex`'s font memory is per default only
of `8,000,000` words, and each pass consumes `500,000` words, so at most
`15` passes can be done before exhausting the memory. And with my
relatively slow hardware, already more than `25` passes are done.

To enlarge the `pdftex` static memory, make sure that the repertory contains
the contributed file `texmf.cnf` and do `export TEXMFCNF="$(pwd):"` and then
try again.

As a shortcut, use `/bin/sh runpdftex.sh` rather which avoids leaving
`TEXMFCNF` set in the environment (which `context` does not like).

See `texmf.cnf` for additional information.
