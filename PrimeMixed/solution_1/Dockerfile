FROM golang:1.27-alpine3.24

WORKDIR /opt/app

COPY *.go build.sh run.sh ./

RUN apk add --no-cache gcc musl-dev && sh ./build.sh

ENTRYPOINT [ "sh", "./run.sh" ]