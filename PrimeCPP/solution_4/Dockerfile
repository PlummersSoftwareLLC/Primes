FROM ubuntu:21.04 AS build

RUN apt update -qq && apt install -y gcc-11 g++-11 clang-12 make

WORKDIR /opt/app

COPY Makefile ./
COPY *.cpp ./
COPY *.hpp ./

ENV CXX=clang++-12
#ENV CXX=g++-11

ENTRYPOINT [ "/usr/bin/make", "run" ]
