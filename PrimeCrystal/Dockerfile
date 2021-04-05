FROM alpine:3.13 AS build

RUN apk add --no-cache crystal bash build-base

WORKDIR /opt/app
COPY primes.cr .
COPY run.sh .

RUN crystal build --release --static --no-debug --verbose primes.cr

ENTRYPOINT [ "./run.sh" ]