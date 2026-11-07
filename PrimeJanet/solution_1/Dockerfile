FROM alpine:3.24

RUN apk add --no-cache janet

WORKDIR /primes

COPY . ./

ENTRYPOINT ["./run.sh"]
