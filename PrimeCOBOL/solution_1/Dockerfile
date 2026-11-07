FROM alpine:3.24

RUN apk add --no-cache gnucobol

WORKDIR /opt/app

COPY ./primes.cbl .
RUN cobc -x primes.cbl

ENTRYPOINT ["./primes"]
