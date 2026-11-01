FROM alpine:3.24

RUN apk add --no-cache build-base nim

WORKDIR /opt/app
COPY primes.nim .
RUN nim c -d:release primes.nim

ENTRYPOINT [ "/opt/app/primes" ]
