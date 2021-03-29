FROM debian:10-slim AS build

RUN apt-get update -qm \
  && apt-get install -qy build-essential

WORKDIR /opt/primes
COPY PrimeCPP.cpp .
RUN g++ -Ofast -std=c++17 PrimeCPP.cpp -oprimes

FROM debian:10-slim

COPY --from=build /opt/primes/primes /usr/bin/primes
CMD ["/usr/bin/primes"]
