FROM golang:1.16-alpine3.13

WORKDIR /opt/app

COPY go.mod .
COPY main.go .

ENTRYPOINT [ "go", "run", "/opt/app/main.go" ]