FROM golang:1.27-alpine3.24

WORKDIR /opt/app

COPY go.mod main.go ./

RUN go build main.go

ENTRYPOINT [ "./main" ]