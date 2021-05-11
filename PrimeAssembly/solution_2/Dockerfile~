FROM alpine:3.13

RUN apk add --update build-base

WORKDIR /opt/app

COPY *.s ./

RUN gcc primes_arm64.s -o primes_arm64

ENTRYPOINT [ "./primes_arm64" ]


