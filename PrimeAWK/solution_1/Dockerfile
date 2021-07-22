FROM ubuntu:20.04

RUN apt-get update -qq \
    && apt-get install -y build-essential mawk

WORKDIR /home/primes

COPY primes.awk counts.csv ./

ENTRYPOINT ["mawk",  "-f", "primes.awk", "-F", ",", "counts.csv"]
