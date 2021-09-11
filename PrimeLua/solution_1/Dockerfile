FROM alpine:3.13

RUN apk add --no-cache lua5.3=5.3.6-r0

WORKDIR /opt/app
COPY Primes.lua .

ENTRYPOINT [ "lua5.3", "Primes.lua" ]
