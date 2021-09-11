FROM alpine:3.13

RUN apk add --no-cache build-base=0.5-r2 perl=5.32.0-r0 perl-dev=5.32.0-r0
WORKDIR /opt/app
COPY *.pl .

ENTRYPOINT [ "perl", "primes.pl" ]