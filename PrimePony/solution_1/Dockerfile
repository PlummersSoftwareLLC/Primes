FROM ghcr.io/ponylang/ponyc:0.77.2 AS build

WORKDIR /opt/app
COPY primes.pony .

RUN ponyc . -b primes --static

FROM alpine:3.24

COPY --from=build /opt/app/primes /usr/local/bin

ENTRYPOINT [ "primes" ]