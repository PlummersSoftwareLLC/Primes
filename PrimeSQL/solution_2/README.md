# Implementations in MariaDB

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)

This solution consists of 3 implementations of the prime calculation in SQL using [MariaDB](https://mariadb.org). The characteristics of each implementation are described below.

## Implementation details

### Overview

These implementations are based on the [SQLite implementation](../solution_1/). The docker image that is used as a basis uses `docker-entrypoint.sh` as an entry point. This script is not used to avoid initializations that are not needed to run the implementations. Instead the script `run.sh` is used to do only the minimal required steps needed to initialize MariaDB and run the implementations.

In al cases memory stored tables without transaction support are used. To enable this and to avoid the table to be written to disk the setting `max_heap_table_size` was set to 1Gb.

Furthermore the `with recursive` statement is used in all implementations. The number of recursive calls is however limited in the default installation. This can result incorrect calculations, without errors. To allow for the correct number of recursions the `max_recursive_iterations` parameter is set to 1.000.000. Note that when the `max_limit` is increased then this parameter needs to be increased too.

### MariaDB1 implementation

This implementation uses the base algorithm. It makes use of a template table `primes_table_template`. This table has two columns. The first column (`n`) contains the natural numbers that are considered and the second column (`isPrime`) is always initialized to the value 1. In the beginning of the calculation this table is copied to a new table `primes_table`. The non prime numbers are eliminated on this table by setting the `isPrime` value to 0.

For the `isPrime` column the datatype `INT` is used. This datatype uses 32 bits for storage. Various experiments revealed that different types like `TINYINT` and `BIT(1)` result in a worse performance.

### MariaDB2 implementation

This implementation does not use the base algorithm. It is as close as possible to the [SQLite solution 1](../solution_1/) implementation and algorithm.

### MariaDB3 implementation

This implementation does not use the base algorithm. It is based on the MariaDB2 implementation and algorithm. Major difference is that it makes use of a fixed table with the numbers that are considered. This allows for one or two more passes compared to the MariaDB2 implementation.

## Run instructions

### Run native

To run this solution you need MariaDB. This database engine is available in most package managers of Linux distributions. The scripts are designed to run in batch mode with `mysql`.

```bash
cd path/to/sieve
mysql -u<user> -p<password> < primes_1.sql
mysql -u<user> -p<password> < primes_2.sql
mysql -u<user> -p<password> < primes_3.sql
```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t maria:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  maria:latest 
    ```

Or do step 2 and 3 with `go.sh`.

## Output

Below is an example of the output on my machine, running with Docker.

```bash
Passes: 1, Time: 10.611, Avg: 10.611 (sec/pass), Limit: 1000000, Count: 78498, Valid: True

fvbakel_MariaDB1;1;10.611;1;algorithm=base,faithful=no,bits=32


Passes: 3, Time: 6.559, Avg: 2.1863333333333332 (sec/pass), Limit: 1000000, Count: 78498, Valid: True

fvbakel_MariaDB2;3;6.559;1;algorithm=other,faithful=no,bits=32


Passes: 4, Time: 5.194, Avg: 1.2985 (sec/pass), Limit: 1000000, Count: 78498, Valid: True

fvbakel_MariaDB3;4;5.194;1;algorithm=other,faithful=no,bits=32
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- MariaDB: 10.6.1
- running in Docker container MariaDB:10.6.2
- Docker version 20.10.2, build 20.10.2-0ubuntu2
