FROM golang:1.16 AS build

WORKDIR /opt/primes
COPY PrimeGO.go .
RUN go build -o primes PrimeGO.go

FROM debian:10-slim

COPY --from=build /opt/primes/primes /usr/bin/primes
CMD ["/usr/bin/primes"]
