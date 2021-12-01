FROM golang:1.17-alpine3.13

WORKDIR /opt/app

COPY . .

RUN go generate && go build -gcflags="-B"

ENTRYPOINT [ "./primego" ]