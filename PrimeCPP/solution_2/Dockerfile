FROM ubuntu:21.04 AS build

RUN apt-get update -qq \
    && apt-get install -y g++

COPY *.cpp /opt/app/
WORKDIR /opt/app
RUN g++ -pthread -Ofast -std=c++17 PrimeCPP_PAR.cpp -oprimes_par

FROM ubuntu:21.04
COPY --from=build /opt/app/primes_par /usr/local/bin

ENTRYPOINT [ "primes_par", "-l", "1000000" ]