FROM golang:1.16-alpine3.13

WORKDIR /opt/app

COPY *.go build.sh run.sh ./

RUN apk add --no-cache gcc musl-dev && sh ./build.sh

ENTRYPOINT [ "sh", "./run.sh" ]