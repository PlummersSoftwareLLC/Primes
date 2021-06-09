# SQLite implementation

![Deviation](https://img.shields.io/badge/Deviation-approximation-blue)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This implementation is an approximation of the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) algorithm in SQLite. SQL is a [Turing complete](https://en.wikipedia.org/wiki/Turing_completeness) language, however in practice there are some limitations on what can practically be done with SQL in SQLite. These limitations and the nature of the SQL language is the reason that this implementation deviates from the exact algorithm, and thus is an approximation

## Challenges

There are some challenges to overcome when using SQLite and conform to the rules in [CONTRIBUTING.md](../../CONTRIBUTING.md#rules). SQLite lacks some important features:

1. a builtin sqrt function
2. a real good way to run the calculation in a while loop for at least 5 seconds and report on the results
3. to have an outer loop as described in the base rules

This implementation uses Python to solve the lack of features 1 and 2.

The lack of an outer loop is mechanism (feature 3) is solved by:

- Using the `with recursive` statement, which results in a loop
- Calculating all prime numbers in two stages

### Difference in the nature of SQL

Further more, the nature of SQL is different compared to other languages:

- A bit array is not natural in SQL. Instead it is more natural to have sets of records
- One by one operations are not natural in SQL. Instead it is more natural to do mass operations on a (sub)set of records.

### The used algorithm

Based on the above, this implementation uses the following algorithm:

In the first stage all prime numbers smaller than the square root of the limit are calculated. This is done with a brute force method:

1. First all natural numbers smaller than square root of the limit are listed
2. Then all numbers that can not be a prime are listed
3. Then list 2 is subtracted from list 1, these are the first set of primes.

In the second stage the elimination is done for the found prime numbers of the first stage. This is done with the following logic:

1. First all natural numbers between the square root of the limit and the limit are listed
2. For all primes that are found in stage 1, all factors are calculated and listed
3. Then list 2 is subtracted from list 1 and the primes found in stage 1 are added
4. The results from 3 are stored in a table, and this is considered the end result.  

### Alternative algorithms

Alternative algorithms and formulations for this implementation have been considered. The different variants are stored in the [attic](./attic/) of this solution.

### Credits

The Python part is based on the implementation from "Python/solution_2, by ssovest".

## Run instructions

### Run native

To run this solution you need Python3. Python3 (`python3`) is included in most Linux distributions.

```bash
cd path/to/sieve
python3 sqlite_runner.py
```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t sqlite:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  sqlite:latest 
    ```

## Output

Below is an example of the output on my machine, running with Docker.

```bash
Passes: 4, Time: 6.110446627000783, Avg: 1.5276116567501958, Limit: 1000000, Count: 78498, Valid: True

fvbakel_sqlite; 4;6.110446627000783;1;algorithm=other,faithful=no,bits=8
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- Python3: 3.9.5
- sqlite3: 3.34.1
- python sqlite module: 2.6.0
- running in Docker container alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2
