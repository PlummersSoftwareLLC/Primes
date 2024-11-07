FROM ubuntu:22.04 AS build

RUN apt-get update -qq \
    && apt-get install -y bash clang

WORKDIR /opt/app
COPY *.cpp .
RUN clang++ -march=native -mtune=native -pthread -Ofast -std=c++17 PrimeCPP_array.cpp -oprimes_array \
    && clang++ -march=native -mtune=native -pthread -Ofast -std=c++17 PrimeCPP_mask.cpp -oprimes_mask

FROM ubuntu:22.04

COPY --from=build /opt/app/primes_array /opt/app/primes_mask /opt/app/

WORKDIR /opt/app
COPY benchmark.sh .

ENTRYPOINT [ "./benchmark.sh"]
CMD ["both", "-l", "1000000" ]