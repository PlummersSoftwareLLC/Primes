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

- the base one with sieving out starting at `factor * factor`
and going by steps of `2 * factor`.
- and a refinement using the "480 out of 2310" wheel (`2*3*5*7*11`). With `pdftex`
this runs almost 3X times faster than the odd-only algorithm, but with `luatex`
the improvement is more like 2.2X, for unknown reasons.

Even configuring `pdftex` (which has no dynamic memory allocation) to its
maximal TeXLive memory setting, a maximum of `294` passes can be done with it
for the sieving range of `1,000,000` (each pass consuming about `500,000`
words of font memory).

As at my locale I achieve with `pdftex` `23` passes for the wheel
implementation (compared to `8` for the base implementation), a computer about
`13` times faster than mine would exhaust `pdftex` maximal font memory during
the benchmark.

For this reason the Dockerfile is configured to run only the `luatex`
benchmark as `luatex` binary has dynamic memory allocation.  Each pass during
the benchmark lets `luatex` allocate again needed memory.


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
DDR3 of memory (mid-2012 machine).

Docker run:

```
jfbu-tex;10;5.38837;1;algorithm=base,faithful=no
jfbu-tex-480of2310;23;5.04814;1;algorithm=wheel,faithful=no,bits=32
```

Native run (with `luatex`: `/bin/sh run.sh`):

```
jfbu-tex;10;5.39767;1;algorithm=base,faithful=no
jfbu-tex-480of2310;22;5.22046;1;algorithm=wheel,faithful=no,bits=32
```

Native run (with `pdftex`: `/bin/sh runpdftex.sh`):

```
jfbu-tex;8;5.19325;1;algorithm=base,faithful=no
jfbu-tex-480of2310;23;5.19388;1;algorithm=wheel,faithful=no,bits=32
```

I don't know why the speed increase from base (one out of two) to wheel (480
out of 2310) is almost 3 with `pdftex` and lower with `luatex`. Also for some
reason the Docker runs I did `pdftex` were about `15%` slower than my native
runs, possibly having to do with the fact that I compiled the `pdftex` binary
locally but not the `luatex`.

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

`erato_primestofile.tex` is a file loading `erato_sieve.tex`, which you can run
with either Knuth base `tex` or `pdftex` or `luatex` to generate files with
one prime per line.  By default it generates `listofprimes-1000000.txt`.

`wheel_primestofile.tex` is a file loading `wheel_sieve.tex`, which you can run
with either Knuth base `tex` or `pdftex` or `luatex` to generate files with
one prime per line using the wheel algorithm.
By default it generates `listofprimes-1000000.txt`.

`{erato,wheel}_primestopdf_{h,v}.tex` are to be compiled with `pdflatex` or
`lualatex`.  They either use or re-compute `listofprimes-1000000.txt` (or some
other configurable range) and then output to `pdf` the prime numbers in a
column-wise manner, the columns being filled from left to right for the `h`
version and from top to bottom for the `v` version.

## More info on native runs with pdftex

Executing

```
pdftex erato_benchmark && cat erato_benchmark-out.txt
```

will most certainly fail, except if your computer is very slow.

To fix this, make sure that the repertory contains the contributed file
`texmf.cnf` and do `export TEXMFCNF="$(pwd):"` and then try again.

See `texmf.cnf` for additional information.
