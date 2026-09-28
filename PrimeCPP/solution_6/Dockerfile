# ---- build stage ----
FROM ubuntu:22.04 AS build

RUN apt-get update -qq \
 && apt-get install -y --no-install-recommends g++ make \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY PrimeCPP.cpp .

RUN g++ -O3 -march=native -mtune=native -pthread -std=c++17 PrimeCPP.cpp -o primes.exe

# ---- runtime stage ----
FROM ubuntu:22.04

WORKDIR /opt/app
COPY --from=build /opt/app/primes.exe /opt/app/

ENTRYPOINT ["./primes.exe"]
