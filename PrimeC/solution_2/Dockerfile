FROM ubuntu:24.04

# Get GCC and Clang
RUN apt-get update && apt-get install -y build-essential clang lld \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/primes

COPY compile.sh run.sh *.c ./

RUN ./compile.sh

ENTRYPOINT [ "./run.sh" ]