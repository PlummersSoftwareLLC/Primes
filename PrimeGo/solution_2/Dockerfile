FROM golang:1.27-alpine3.24

WORKDIR /opt/app

COPY . .

RUN go generate && go build -gcflags="-B"

ENTRYPOINT [ "./primego" ]