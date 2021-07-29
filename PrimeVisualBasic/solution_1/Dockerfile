FROM ubuntu:20.04

RUN apt-get update && apt-get install -y mono-vbnc

COPY Primes.vb /opt
WORKDIR /opt
RUN vbnc Primes.vb > /dev/null 2>&1

ENTRYPOINT [ "mono", "Primes.exe" ]
