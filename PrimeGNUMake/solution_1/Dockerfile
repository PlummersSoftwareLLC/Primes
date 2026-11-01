FROM alpine:3.24

WORKDIR /opt/app

COPY makefile .
COPY gmsl/* ./gmsl/

RUN apk add --no-cache  make

ENTRYPOINT [ "make" ]