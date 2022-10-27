# Build

FROM ubuntu:22.04 AS build

RUN apt-get update -qq && apt-get install -y g++-11 clang-12 make

WORKDIR /opt/app

COPY Makefile *.cpp *.hpp ./

ENV CXX_CLANG=clang++-12 \
    CXX_GCC=g++-11

RUN make -j bench

# Run

FROM ubuntu:22.04

RUN apt-get update -qq && apt-get install -y make \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY --from=build /opt/app/* ./

ENTRYPOINT [ "/usr/bin/make", "-j", "run_bench" ]