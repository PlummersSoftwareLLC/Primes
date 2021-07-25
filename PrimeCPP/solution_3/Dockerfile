FROM ubuntu:21.04 AS build

RUN apt-get update -qq \
    && apt-get install -y clang

COPY *.cpp *.h /opt/app/
WORKDIR /opt/app
RUN clang++ -pthread -O3 -m64 -mtune=native -fconstexpr-steps=1000000000 -fconstexpr-depth=100000000 -std=c++20 PrimeCPP_CONSTEXPR.cpp -oprimes_constexpr

FROM ubuntu:21.04
COPY --from=build /opt/app/primes_constexpr /usr/local/bin

ENTRYPOINT [ "primes_constexpr" ]