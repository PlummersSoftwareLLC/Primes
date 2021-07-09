FROM ubuntu:18.04 AS build

RUN apt-get update -qq \
    && apt-get install -y clang-10

COPY *.cpp *.h /opt/app/
WORKDIR /opt/app
RUN clang++-10 -pthread -O3 -m64 -mtune=native -std=c++20 PrimeCPP_CONSTEXPR.cpp -oprimes_constexpr

FROM ubuntu:18.04
COPY --from=build /opt/app/primes_constexpr /usr/local/bin

ENTRYPOINT [ "primes_constexpr" , "1000000"]