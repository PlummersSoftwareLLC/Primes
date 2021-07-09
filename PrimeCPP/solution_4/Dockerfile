FROM ubuntu:21.04 AS build

RUN apt update -qq && apt install -y clang-12 make

WORKDIR /opt/app

COPY Makefile ./
COPY *.cpp ./
COPY *.hpp ./

ENV CXX=clang++-12

ENTRYPOINT [ "/usr/bin/make", "-j", "run" ]
