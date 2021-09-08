FROM ubuntu:20.04

RUN apt-get update -qq && \
    apt-get install -y build-essential=12.8ubuntu1.1 mawk=1.3.4.20200120-2 --no-install-recommends && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /home/primes

COPY primes.awk counts.csv ./

ENTRYPOINT ["mawk",  "-f", "primes.awk", "-F", ",", "counts.csv"]
