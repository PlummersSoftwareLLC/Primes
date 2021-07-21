FROM ubuntu:20.04 as build

RUN apt-get update -qq && apt-get install -y build-essential --no-install-recommends && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /home/primes

COPY sieve_memcopy.c run.sh ./

RUN chmod a+x run.sh

ENTRYPOINT ["./run.sh"]
