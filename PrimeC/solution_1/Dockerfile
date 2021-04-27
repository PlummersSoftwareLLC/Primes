FROM ubuntu

# Get GCC
RUN apt-get update
RUN apt-get install -y build-essential

WORKDIR /home/primes

COPY sieve.c prime-check.h run ./

ENTRYPOINT ["./run"]
