FROM ubuntu:20.04

RUN apt-get update && apt-get -y install build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/primes

COPY *.sh *.c ./

RUN ./compile.sh

ENTRYPOINT [ "./run.sh" ]