FROM alpine:3.13

RUN apk add --no-cache perl bash build-base

WORKDIR /opt/app
COPY primes.pl .
COPY run.sh .

ENTRYPOINT [ "./run.sh" ]