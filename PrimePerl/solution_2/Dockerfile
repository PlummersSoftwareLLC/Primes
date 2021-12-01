FROM alpine:3.13

RUN apk add --no-cache build-base perl perl-dev
WORKDIR /opt/app
COPY *.pl .

ENTRYPOINT [ "perl", "primes.pl" ]