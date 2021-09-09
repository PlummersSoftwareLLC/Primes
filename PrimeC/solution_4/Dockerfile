FROM ubuntu:20.04

# Get GCC
RUN apt-get update && apt-get install --no-install-recommends -y build-essential=12.8ubuntu1.1 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/primes

COPY sieve.c prime-check.h run ./

ENTRYPOINT ["./run"]
