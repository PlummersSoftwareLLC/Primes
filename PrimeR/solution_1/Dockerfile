FROM alpine:3.13

RUN apk add --no-cache R

WORKDIR /opt/app

COPY primes.R .

ENTRYPOINT ["Rscript", "primes.R"]
