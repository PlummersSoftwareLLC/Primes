FROM alpine:3.24

RUN apk add --no-cache luajit

WORKDIR /prog
COPY prog.lua .
COPY l1_sieve.lua .
COPY compiled/ ./compiled

ENTRYPOINT [ "luajit", "prog.lua", "b" ]
