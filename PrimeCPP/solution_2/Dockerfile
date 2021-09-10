FROM ubuntu:21.04 AS build

RUN apt-get update -qq \
    && apt-get install --no-install-recommends -y clang=1:12.0-52~exp1

WORKDIR /opt/app
COPY *.cpp .
RUN clang++ -march=native -mtune=native -pthread -Ofast -std=c++17 PrimeCPP_PAR.cpp -oprimes_par

FROM ubuntu:21.04
COPY --from=build /opt/app/primes_par /usr/local/bin

ENTRYPOINT [ "primes_par", "-l", "1000000" ]