FROM ponylang/ponyc:0.41.1-alpine AS build

WORKDIR /opt/app
COPY primes.pony .

RUN ponyc . -b primes --static

FROM alpine:3.13

COPY --from=build /opt/app/primes /usr/local/bin

ENTRYPOINT [ "primes" ]