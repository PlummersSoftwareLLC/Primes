# AWK Solution
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## What is AWK?
[AWK](https://en.wikipedia.org/wiki/AWK) is a scripting language created by Alfred **A**ho, Peter **W**einberger and Brian **K**ernighan for simple text processing.

## Implementation
The algorithm is the same showed by Dave Plummer in his C++ implementation



## Run

### Docker
```
$ docker build -t drag-race .
$ docker run drag-race
```

### Linux
Awk comes preinstalled with most Linux systems

```
$ awk -F "," -f primes.awk counts.csv

```
Awk needs a file to operate on and to create an associative array with the counts.

## Benchmarks

Some results I've got in my machine

```
Passes: 14, Time: 5, Average: 0.357143, Count: 78498, Valid: 1
Passes: 11, Time: 5, Average: 0.454545, Count: 78498, Valid: 1
Passes: 13, Time: 5, Average: 0.384615, Count: 78498, Valid: 1
Passes: 12, Time: 5, Average: 0.416667, Count: 78498, Valid: 1
Passes: 13, Time: 5, Average: 0.384615, Count: 78498, Valid: 1

```

## Author
[Davi Nakamura](https://github.com/DaviNakamuraCardoso)
