FROM ubuntu:22.04 AS build

RUN apt-get update -qq \
    && apt-get install -y algol68g build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY primes.a68 .
RUN a68g -O3 --compile primes.a68

FROM ubuntu:22.04
COPY --from=build /opt/app/primes.sh /usr/local/bin

ENTRYPOINT [ "primes.sh" ]

