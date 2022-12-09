FROM ubuntu:22.04 AS build

RUN apt-get update && apt-get install -y wget \
  && rm -rf /var/lib/apt/lists/*

RUN wget https://github.com/terralang/terra/releases/download/release-1.0.6/terra-Linux-x86_64-6184586.tar.xz -P /tmp/

RUN apt-get update && apt-get install -y tar \
  && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y xz-utils \
  && rm -rf /var/lib/apt/lists/*

RUN tar -xvf /tmp/terra-Linux-x86_64-6184586.tar.xz -C /tmp/

RUN apt-get update \
 && apt-get install -y sudo

RUN adduser --disabled-password --gecos '' docker
RUN adduser docker sudo


RUN apt-get update
RUN sudo apt-get install -y build-essential llvm-11-dev libclang-11-dev clang-11 libedit-dev libncurses5-dev zlib1g-dev libpfm4-dev



COPY *.t ./
RUN sudo /tmp/terra-Linux-x86_64-6184586/bin/terra Primes.t

USER docker

CMD ["./Sieve"]