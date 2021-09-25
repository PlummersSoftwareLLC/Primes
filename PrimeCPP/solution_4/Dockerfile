# Build

FROM ubuntu:21.04 AS build

RUN apt-get update -qq && apt-get install -y g++-11 clang-12 make

WORKDIR /opt/app

COPY Makefile ./
COPY *.cpp ./
COPY *.hpp ./

ENV CXX_CLANG=clang++-12
ENV CXX_GCC=g++-11

RUN make -j bench

# Run

FROM ubuntu:21.04

RUN apt-get update -qq && apt-get install -y make

WORKDIR /opt/app

COPY --from=build /opt/app/* ./

ENTRYPOINT [ "/usr/bin/make", "-j", "run_bench" ]
