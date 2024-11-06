FROM ubuntu:22.04 AS build

RUN apt-get update -qq \
    && apt-get install -y clang

WORKDIR /opt/app
COPY *.cpp .
RUN clang++ -march=native -mtune=native -pthread -Ofast -std=c++17 PrimeCPP_PAR.cpp -oprimes_par

FROM ubuntu:22.04
COPY --from=build /opt/app/primes_par /usr/local/bin

ENTRYPOINT [ "primes_par", "-l", "1000000" ]