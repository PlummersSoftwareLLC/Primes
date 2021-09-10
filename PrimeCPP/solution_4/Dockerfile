# Build

FROM ubuntu:21.04 AS build

RUN apt-get update -qq && apt-get install --no-install-recommends -y g++-11=11.1.0-1ubuntu1~21.04 clang-12=1:12.0.0-3ubuntu1~21.04.1 make=4.3-4ubuntu1

WORKDIR /opt/app

COPY Makefile *.cpp *.hpp ./

ENV CXX_CLANG=clang++-12
ENV CXX_GCC=g++-11

RUN make -j bench

# Run

FROM ubuntu:21.04

RUN apt-get update -qq && apt-get install --no-install-recommends -y make=4.3-4ubuntu1 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY --from=build /opt/app/* ./

ENTRYPOINT [ "/usr/bin/make", "-j", "run_bench" ]