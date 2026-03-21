FROM ubuntu:22.04

RUN apt-get update -qq \
    && apt-get install -y algol68g gcc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY primes*.a68 run_primes.sh ./
RUN a68g -O3 --compile primes.a68 && \
    a68g -O3 --compile primes_bit.a68

ENTRYPOINT [ "./run_primes.sh" ]

