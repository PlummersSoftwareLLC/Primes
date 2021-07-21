FROM alpine:3.13 as builder

RUN apk update && \
    apk add curl make g++

WORKDIR /opt

RUN curl https://github.com/kos-lang/kos/releases/download/v0.0.31/kos-0.0.31-src.tar.gz -O -L && \
    tar xzf kos-0.0.31-src.tar.gz

WORKDIR /opt/kos-0.0.31-src

RUN make install -j"$(nproc)"

WORKDIR /opt

RUN rm -rf kos-0.0.31-src

WORKDIR /opt/app

COPY primes.kos .

ENTRYPOINT [ "./primes.kos" ]
