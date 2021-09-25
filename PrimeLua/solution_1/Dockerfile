FROM alpine:3.13

RUN apk add --no-cache lua5.3

WORKDIR /opt/app
COPY Primes.lua .

ENTRYPOINT [ "lua5.3", "Primes.lua" ]
