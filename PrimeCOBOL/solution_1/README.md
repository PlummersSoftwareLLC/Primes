# COBOL implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This is an implementation in COBOL. It makes use of the GNUCobol compiler. An array of the data type `PIC 1(1)` is used, which comes down to a 8 bit storage in the GNUCobol compiler.

## Considerations

In COBOL it is common to have globals declared with a fixed size. This static declaration is used in this implementation which makes it not faithful. In each run of the calculation this static array is filled with ones. Setting these one by one is very CPU intensive and slow operation in COBOL. Instead it is much faster to copy memory. So instead of:

```COBOL
            PERFORM VARYING Z 
                    FROM 1 BY 1 
                    UNTIL Z>BIT_SIZE
                 MOVE 1 TO FLAG (Z)
            END-PERFORM.
```

The following code is used:

```COBOL
        01  ONE-FILLED-ARRAY.        
            03 ONE PIC 1(1)
               VALUE 1 
               OCCURS 500000 TIMES
               INDEXED BY Y. 
        ...

        MOVE ONE-FILLED-ARRAY TO BIT-ARRAY.
```

This initialization improvement, improved the performance by 40%!.

## Run instructions

### Build and run native

To run this solution you need the GNUCobol compiler and dependencies.

```bash
cd path/to/sieve
cobc -x primes.cbl
./primes
```

### Run with Docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t cobol:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  cobol:latest 
    ```

Or you can do step 2 and 3 with `go.sh`.

## Output

Below is an example of the output on my machine, running with Docker.

```bash
Passes: 01104, Time: 5.0, Avg: 0.00452 (sec/pass), Limit: 1000000, Count: 0078498, Valid: True 
 
fvbakel_Cobol;01104;5.0;1;algorithm=base,faithful=no,bits=8
```

These results are with the following conditions:

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- GNUCobol Compiler: 3.1
- running in Docker container alpine:3.13
- Docker version 20.10.2, build 20.10.2-0ubuntu2

## Note

This is my first COBOL program, it is quite possible that better implementations are possible. I know that now days it is allowed to use lowercase in COBOL, however I choose to use the uppercase for the nostalgic feeling.
