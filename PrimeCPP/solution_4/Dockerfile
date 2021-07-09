FROM ubuntu:21.04 AS build

RUN apt update -qq && apt install -y g++-11 clang-12 make

WORKDIR /opt/app

COPY Makefile ./
COPY *.cpp ./
COPY *.hpp ./

ENV CXX_CLANG=clang++-12
ENV CXX_GCC=g++-11

ENTRYPOINT [ "/usr/bin/make", "-j", "run_all" ]
