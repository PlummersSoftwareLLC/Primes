# LaTeX Prime Sieve by tjol

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)

This is an port of Dave's prime sieve to LaTeX with `expl3` (experimental
LaTeX 3 features).

I've never used LaTeX 3 before, so it's safe to say the code is not idiomatic.
There is no encapsulation at all, so the implementation is not faithful (I'm
not even sure if class-like encapsulation is possible, LaTeX is just a
document processer after all).

## Files

`prime_functions.tex` contains the actual implementation.

`prime_race.tex` is the main program.

`prime_numbers.tex` generates a document with the prime numbers up to 100'000.
(Not 1'000'000, laying all of them out on the page is too much)


## How to run

Any up-to-date (2021) TeX distribution should work. Tested with texlive-2021 on
Linux.

Run

```
pdflatex prime_race.tex
```

To generate a PDF file, `prime_race.pdf`, with the results. This also writes a
text file `result.txt`, with the result in the standard format.

Alternatively, `run.sh` will suppress the LaTeX output and copy `result.txt`
to standard output.

## Output

(Copied from the PDF)

|                  |         |
| ---------------- | ------- |
| **Primes found** | 78498   |
| **Limit**        | 1000000 |
| **Total time**   | 13.08 s |
| **Passes**       | 1       |
| **Avg time**     | 13.08 s |
| **Valid**        | Yes     |

This is on a Ryzen 5 2600 running OpenSUSE Tumbleweed.

