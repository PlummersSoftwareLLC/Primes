FROM alpine:3.13

RUN apk add --update tcl

WORKDIR /opt/app

COPY oo_primes.tcl .

ENTRYPOINT ["tclsh","oo_primes.tcl" ]
