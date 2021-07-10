FROM ubuntu:18.04 AS build

RUN apt-get update -qq \
    && apt-get install -y clang-10

COPY *.cpp *.h /opt/app/
WORKDIR /opt/app
RUN clang++-10 -pthread -O3 -m64 -mtune=native -std=c++17 generator.cpp -ogenerator
RUN /opt/app/generator > primes.h
RUN clang++-10 -pthread -O3 -m64 -mtune=native -std=c++17 fastware.cpp -ofastware

FROM ubuntu:18.04
COPY --from=build /opt/app/fastware /usr/local/bin

ENTRYPOINT [ "fastware" ] 