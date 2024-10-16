FROM crystallang/crystal:1.14-alpine AS build

WORKDIR /opt/app
COPY primes.cr .
RUN crystal build primes.cr --release --static --no-debug

FROM alpine:3

COPY --from=build /opt/app/primes /usr/local/bin/

ENTRYPOINT [ "primes" ]
