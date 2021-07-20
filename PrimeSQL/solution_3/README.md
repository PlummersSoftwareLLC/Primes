# Implementations in Oracle Database XE

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)

This solution consists of 2 implementations of the prime calculation in SQL (PL/SQL) using [Oracle Database XE](https://www.oracle.com/database/technologies/appdev/xe.html). This is a special edition of Oracle database that is free to use for educational and personal usage, with a number of limitations in place. Note that Oracle also provides different editions of their database engine that are not free and can therefor not be used in this solution. For example [Oracle Database Enterprise](https://www.oracle.com/database/enterprise/). This solution should also work on those editions and might have a better performance under these conditions. The characteristics of each implementation are described below.

## Implementation details

### Oracle1 implementation

This implementation uses the base algorithm. It makes use of Oracle types which are similar to a `struct` in C. The statements below show the declaration of the types.

```SQL
CREATE or REPLACE TYPE flag_t as OBJECT (isPrime number(1));
CREATE or REPLACE TYPE bit_tab as TABLE of flag_t;
```

A new instance is created by `bit_array := bit_tab();`.

### Oracle2 implementation

This implementation does not use the base algorithm. It is as close as possible to the [SQLite solution 1](../solution_1/) implementation and algorithm. The actual calculation makes use of only SQL statements.

## Run instructions

### Run native

To run this solution you need an Oracle database and a working `sqlplus` client.

```bash
cd path/to/sieve
sqlplus -F -S system/oracle <primes_1.sql
sqlplus -F -S system/oracle <primes_2.sql
```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t oracle:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  oracle:latest 
    ```

Or do step 2 and 3 with `go.sh`.

## Output

Below is an example of the output on my machine, running with Docker.

```bash
Passes: 4, Time: 5.050337, Avg: 1.26258425 (sec/pass), Limit: 1000000, Count: 78498, Valid: True

fvbakel_Oracle1;4;5.050337;1;algorithm=base,faithful=yes,bits=32


Passes: 1, Time: 9.384464, Avg: 9.384464 (sec/pass), Limit: 1000000, Count: 78498, Valid: True

fvbakel_Oracle2;1;9.384464;1;algorithm=other,faithful=no,bits=32
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- Oracle XE: 18.4.0.0
- Running in Docker container 
- Docker version 20.10.2, build 20.10.2-0ubuntu2

## Known issues

The Docker image will take some time to setup on the first run. Next to that, the Oracle database has to be started on each run. Starting the database can take up to one minute and during that time it might seem that the calculation hangs.
