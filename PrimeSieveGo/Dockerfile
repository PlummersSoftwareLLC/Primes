FROM golang:1.16 AS builder

COPY go.mod /app/
COPY main.go /app/

WORKDIR /app/
RUN go build ./


FROM ubuntu:20.04

COPY --from=builder /app/PrimeSieveGo /usr/local/bin

ENTRYPOINT ["PrimeSieveGo"]
