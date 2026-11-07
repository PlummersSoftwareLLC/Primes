FROM alpine:3.24

RUN apk add --no-cache luajit

WORKDIR /app

COPY PrimeLua.lua .

ENTRYPOINT [ "luajit", "PrimeLua.lua" ]
