# Sieve of Eratosthenes implemented in (functional) R

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)


This implementation of the prime sieve is less faithful to the original C++ object-oriented design, going for a 
functional design, which is more commonplace for R. Part of R's power comes through vectorised operations rather than
relying on R's built-in for loops, which are usually slow. Vectorised R code typically translates to C-style for loops, 
leading to much more efficient code. More on this [here](https://adv-r.hadley.nz/perf-improve.html#vectorise).
I'd like to thank @fvbakel for his [R solution](../solution_1), which is similar, but with a OOP design. I have adapted his
`runSieve()` function into my solution for efficiency.

As mentioned by @fvbakel, the author of R [solution 1](../solution_1), R's variables are all stored as vectors, and hence
are of variable size depending on the size of the vector (due to the overhead of the vector structure itself). For a vector of length 1,000,000,
each stored int takes 32 bits.


## Run

### Run locally

R is available on Linux, MacOSX and, Windows via [a variety of mirrors](https://cran.r-project.org/mirrors.html).
The script was tested using R version 4.1.0 on Ubuntu 20.04 LTS, and R version 3.6.3 on Windows 10 20H2. It should 
run on ARM machines as well via the Docker image, however this is untested.
On Ubuntu, you can install base R using:
```
sudo apt install r-base
```


### Docker

1. Build the Docker image
```
docker build -t primes-r .
```
2. Run the Docker image
```
docker run primes-r
```

## Output

For full output with all prime numbers, set the first argument of `printResults()` on line 104 in [primes.R](./primes.R#L104) to `TRUE`.
This will output the primes to stderr. Regular output (with number of passes etc.) is output to stdout.

Example Docker output from my machine:
```
sudo docker run primes-r
"Passes: 673, Time: 5.000000, Avg: 0.007429, Limit: 1000000, Count: 78498, Valid: TRUE"
"nobrien97;673;5.000000;1;algorithm=base,faithful=no,bits=32"
```

Running on:
- i7 4770 @ 3.4GHz
- Ubuntu 20.04 LTS via WSL 2 on Windows 10 20H2
- Docker 20.10.2