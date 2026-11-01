# Build

FROM ubuntu:24.04 AS build

RUN apt-get update -qq && apt-get install -y g++-13 clang-18 make

WORKDIR /opt/app

COPY Makefile *.cpp *.hpp ./

ENV CXX_CLANG=clang++-18 \
    CXX_GCC=g++-13

RUN make -j bench

# Run

FROM ubuntu:24.04

RUN apt-get update -qq && apt-get install -y make \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY --from=build /opt/app/* ./

ENTRYPOINT [ "/usr/bin/make", "-j", "run_bench" ]