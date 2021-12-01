FROM alpine:3.13

RUN apk add --no-cache --update tcl

WORKDIR /opt/app

COPY primes.tcl .

ENTRYPOINT [ "tclsh","primes.tcl" ]