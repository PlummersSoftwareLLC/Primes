# Implementation in MariaDB

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)

## Run instructions

### Run native

To run this solution you need MariaDB. This database engine is available in most package managers of Linux distributions. The script is designed to run in batch mode with `mysql`.

```bash
cd path/to/sieve
mysql < primes.sql
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

```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- MariaDB: 10.6.1
- running in Docker container MariaDB:10.6.2
- Docker version 20.10.2, build 20.10.2-0ubuntu2
