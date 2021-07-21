FROM crystallang/crystal:1.0.0-alpine AS build

WORKDIR /opt/app
COPY primes.cr .
RUN crystal build primes.cr --release --static --no-debug

FROM alpine:3.13

COPY --from=build /opt/app/primes /usr/local/bin/

ENTRYPOINT [ "primes" ]