FROM golang:1.16-alpine3.13

WORKDIR /opt/app

COPY *.go build.sh run.sh ./

RUN apk add --no-cache gcc=10.2.1_pre1-r3 musl-dev=1.2.2-r1 && sh ./build.sh

ENTRYPOINT [ "sh", "./run.sh" ]