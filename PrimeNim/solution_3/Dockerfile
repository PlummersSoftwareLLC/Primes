FROM alpine:3.24

RUN apk add --no-cache gcc musl-dev nim~2.2

WORKDIR /opt/app
COPY Primes.nim .
RUN nim c --mm:arc -d:danger -t:-march=native -d:lto --threads:on Primes.nim

ENTRYPOINT [ "/opt/app/Primes" ]
