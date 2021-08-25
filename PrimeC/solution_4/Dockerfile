FROM ubuntu:20.04

# Get GCC
RUN apt-get update
RUN apt-get install -y build-essential

WORKDIR /home/primes

COPY sieve.c prime-check.h run ./
RUN chmod a+x run

ENTRYPOINT ["./run"]
