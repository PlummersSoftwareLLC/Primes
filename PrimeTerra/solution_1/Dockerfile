FROM ubuntu:22.04 AS build

WORKDIR /opt/app/

RUN apt-get update && apt-get install -y wget tar xz-utils build-essential llvm-11-dev libclang-11-dev clang-11 libedit-dev libncurses5-dev zlib1g-dev libpfm4-dev\
  && rm -rf /var/lib/apt/lists/* \
  && wget https://github.com/terralang/terra/releases/download/release-1.0.6/terra-Linux-x86_64-6184586.tar.xz -P /tmp/ \
  && tar -xvf /tmp/terra-Linux-x86_64-6184586.tar.xz -C /tmp/ 

COPY *.t ./

RUN /tmp/terra-Linux-x86_64-6184586/bin/terra Primes.t


CMD ["./Sieve"]
